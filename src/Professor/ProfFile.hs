{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Professor.ProfFile
    (
    -- * General types
      ProfLine(..)
    , ProfHeader(..)
    , Prof(..)
    , ProfLineDiff(..)
    , ProfLineSummary(..)
    , TreeProf(..)
    , TreeProfDiff(..)
    , ProfDiff(..)
    , ProfStats(..)
    , Diff(..)
    , Stats(..)
    -- * Various friendly printable types
    , Percent(..)
    , Bytes(..)
    , Seconds(..)
    , makePercent
    -- * Printing & Parsing
    , diffProf
    , treeProfGetLine
    , treeProfGetChildren
    , profLineDiff
    , treeProfGet
    , treeProfDiff
    , parseProf
    , printProfLine
    , treeParseProf
    , statsProfs
    , fqCC
    ) where

import Data.List
import Data.Function (on)
import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Text.Printf
import Data.Word
import Data.Char
import qualified Data.Map as M
import qualified Foundation.Parser as F

newtype Percent = Percent { unPercent :: Double }
    deriving (Eq,Ord,Num,Real,Fractional)
newtype Seconds = Seconds { unSeconds :: Double }
    deriving (Eq,Ord,Num,Real,Fractional)
newtype Bytes = Bytes { unBytes :: Word64 }
    deriving (Eq,Ord,Num,Integral,Enum,Real)
instance Fractional Bytes where
    (/) (Bytes a) (Bytes b) = Bytes (a `div` b)
    fromRational = Bytes . truncate

makePercent :: Real a => a -> a -> Percent
makePercent a b = Percent $ (fromRational (toRational a) * 100 / fromRational (toRational b))

instance Show Bytes where
   show (Bytes i)
      | i > 2^.32  = printf "%.2f" (fromIntegral i / 2^.30 :: Double) ++ " gb"
      | i > 2^.22  = printf "%.1f" (fromIntegral i / 2^.20 :: Double) ++ " mb"
      | i > 2^.12  = printf "%.0f" (fromIntegral i / 2^.10 :: Double) ++ " kb"
      | otherwise = show i ++ "  b"

(^.) :: Num a => a -> Int -> a
(^.) n a = n ^ (a :: Int)

instance Show Seconds where
    show (Seconds s)
        | s >= 0.1   = printf "%02.2f seconds" s
        | otherwise  = printf "%01.3f seconds" s

instance Show Percent where
    show (Percent d) = printf "%03.1f%%" d

--COST CENTRE  MODULE  no.        entries  %time %alloc   %time %alloc
data ProfLine = ProfLine
    { plHeadingSpace    :: {-# UNPACK #-} !Int
    , plCostCenter      :: !String
    , plModule          :: !String
    , plLocation        :: !Loc
    , plNo              :: {-# UNPACK #-} !Int
    , plEntries         :: {-# UNPACK #-} !Int
    , plIndividualTime  :: {-# UNPACK #-} !Percent
    , plIndividualAlloc :: {-# UNPACK #-} !Percent
    , plInheritedTime   :: {-# UNPACK #-} !Percent
    , plInheritedAlloc  :: {-# UNPACK #-} !Percent
    } deriving (Show,Eq)

data Loc =
      NoLoc String
    | Loc String String
    deriving (Show,Eq)

data ProfLineDiff = ProfLineDiff
    { pldEntries        :: !Int
    , pldIndividualTime  :: !Percent
    , pldIndividualAlloc :: !Percent
    , pldInheritedTime   :: {-# UNPACK #-} !Percent
    , pldInheritedAlloc  :: {-# UNPACK #-} !Percent
    } deriving (Show,Eq)

profLineDiff :: ProfLine -> ProfLine -> ProfLineDiff
profLineDiff pl1 pl2 = ProfLineDiff
    { pldEntries         = plEntries pl2 - plEntries pl1
    , pldIndividualTime  = plIndividualTime pl2 - plIndividualTime pl1
    , pldIndividualAlloc = plIndividualAlloc pl2 - plIndividualAlloc pl1
    , pldInheritedTime   = plInheritedTime pl2 - plInheritedTime pl1
    , pldInheritedAlloc  = plInheritedAlloc pl2 - plInheritedAlloc pl1
    }

data ProfLineSummary = ProfLineSummary
    { plsCostCenter :: String
    , plsModule     :: String
    , plsTime       :: Percent
    , plsAlloc      :: Percent
    } deriving (Show,Eq)

data TreeProf = TreeProf ProfLine (Maybe ProfLine) [TreeProf]
    deriving (Show,Eq)

treeProfGetLine :: TreeProf -> ProfLine
treeProfGetLine (TreeProf tp _ _) = tp

treeProfGetChildren :: TreeProf -> [TreeProf]
treeProfGetChildren (TreeProf _ _ l) = l


getTreeProfLine :: TreeProf -> ProfLine
getTreeProfLine (TreeProf pl _ _) = pl

data TreeProfDiff =
      TreeDiffMatch
        { tpLeftLine  :: ProfLine
        , tpRightLine :: ProfLine
        , tpChildren  :: [TreeProfDiff]
        }
    | TreeDiffNotMatch (Either TreeProf TreeProf)
    deriving (Show,Eq)

data ProfHeader = ProfHeader
    { profCommandLine :: String
    , profTotalTime   :: !Seconds
    , profNbTicks     :: !Int
    , profTickSpeed   :: !Int
    , profNbCpus      :: !Int
    , profTotalAlloc  :: !Bytes
    } deriving (Show,Eq)

data Prof = Prof
    { profHeader          :: !ProfHeader
    , profToplevels       :: [ProfLineSummary]
    , profCostCenterLines :: [ProfLine]
    }

data Diff a = Diff
    { diffBaseline :: a
    , diffDelta    :: a
    , diffPercent  :: Percent
    } deriving (Show,Eq)

data Stats a = Stats
    { statAvg    :: a
    , statMin    :: a
    , statMax    :: a
    , statStddev :: a
    } deriving (Eq)

stdDev :: [Double] -> Double
stdDev xs = sqrt $ (sum $ map (^.2) xs) / l - (sum xs / l) ^. 2
    where l = fromIntegral $ length xs

instance Show a => Show (Stats a) where
    show stat = show (statAvg stat) ++ " (stddev=" ++ show (statStddev stat) ++ " min=" ++ show (statMin stat) ++ " max=" ++ show (statMax stat) ++ ")"

stats :: (Ord a, Fractional a, Real a) => [a] -> Stats a
stats l = Stats { statAvg    = (m / realToFrac (toRational len))
                , statMin    = minimum l
                , statMax    = maximum l
                , statStddev = fromRational $ realToFrac $ stdDev $ map (realToFrac . toRational) l
                }
  where
    !m   = sum l
    !len = length l

data ProfDiff = ProfDiff
    { profDiffTotalTime :: Diff Seconds
    , profDiffTicks     :: Diff Int
    , profDiffAlloc     :: Diff Bytes
    } deriving (Show,Eq)

data ProfStats = ProfStats
    { profStatsTime  :: Stats Seconds
    --, profStatsTicks :: Average Int
    , profStatsAlloc :: Stats Bytes
    } deriving (Show, Eq)

treeParseProf :: Maybe ProfLine -> [ProfLine] -> [TreeProf]
treeParseProf _      []     = []
treeParseProf parent (x:xs) =
    let (lead, reminder) = break ((>=) (plHeadingSpace x) . plHeadingSpace) xs
        tprofs           = treeParseProf (Just x) lead
     in TreeProf x parent (moveCafEnd tprofs) : treeParseProf parent reminder
  where 
    moveCafEnd x = let (l1, l2) = partition treeProfIsCAF x in (l2 ++ l1)
    treeProfIsCAF (TreeProf x _ _) = "CAF" `isPrefixOf` plCostCenter x


data M = OnlyLeft TreeProf | OnlyRight [TreeProf] | Both TreeProf [TreeProf]

treeProfDiff :: [TreeProf] -> [TreeProf] -> [TreeProfDiff]
treeProfDiff lefts rights =
    let m = foldl (\acc rv ->
                            let rk = fqCC (getTreeProfLine rv)
                             in M.alter (updateRight rv) rk acc)
                  (M.fromList $ map (\l -> (fqCC (getTreeProfLine l), OnlyLeft l)) lefts)
                  rights
     in map toDiffMatch $ sortByPerf $ M.elems m
  where
    updateRight :: TreeProf -> Maybe M -> Maybe M
    updateRight r (Just (OnlyLeft l))  = Just (Both l [r])
    updateRight r (Just (Both l rs))   = Just (Both l (r:rs))
    updateRight r (Just (OnlyRight rs)) = Just (OnlyRight (r:rs))
    updateRight r Nothing              = Just (OnlyRight [r])

    sortByPerf = reverse . sortBy (compare `on` plInheritedTime . getTreeProfLine . mProfLine)

    mProfLine (OnlyLeft l)      = l
    mProfLine (OnlyRight (r:_)) = r
    mProfLine (Both l _)        = l
    mProfLine _                 = error "mProfLine"

    toDiffMatch :: M -> TreeProfDiff
    toDiffMatch (Both (TreeProf l _ ls) ((TreeProf r _ rs):_)) = TreeDiffMatch l r (treeProfDiff ls rs)
    toDiffMatch (OnlyLeft l)                                   = TreeDiffNotMatch (Left l)
    toDiffMatch (OnlyRight (r:_))                              = TreeDiffNotMatch (Right r)
    toDiffMatch _                                              = error "toDiffMatch"

-- fully qualified Cost Center
fqCC :: ProfLine -> String
fqCC pl = plModule pl ++ "." ++ plCostCenter pl

treeProfGet :: ProfLine -> [TreeProf] -> Maybe TreeProf
treeProfGet profLine = loop
  where
    loop [] = Nothing
    loop (t@(TreeProf pf _ l):xs)
        | profLine == pf = Just t
        | otherwise      = loop l `mplus` loop xs

--treeDiffGet :: ProfLine -> TreeDiff -> Maybe TreeDiff
--treeDiffGet

parseProf :: String -> Prof
parseProf =
      (\(a,(b,c)) -> Prof a b c)
    . (parseHeader *** ((parseToplevels *** parseCC) . splitSection . drop 1))
    . splitSection
    . filter (/= "")
    . lines
  where
    parseHeader :: [String] -> ProfHeader
    parseHeader (_:cmdline:ttLine:taLine:_) =
        let (secs, ticks, tickSpeed, nbCpus) =
                case words ttLine of
                    "total":"time":"=":itsec:"secs":('(':iticks):"ticks":"@":itickSpeed:_:inbCpus:('p':'r':'o':'c':_):[] -> (itsec, iticks, itickSpeed, inbCpus)
                    l -> error ("couldn't parse total time line: " ++ show l)
            nbBytes =
                case words taLine of
                    "total":"alloc":"=":inbBytes:"bytes":_ -> inbBytes
                    l                                      -> error ("couldn't parse total alloc line: " ++ show l)
         in ProfHeader { profCommandLine = cmdline
                       , profTotalTime   = Seconds $ read secs
                       , profNbTicks     = read ticks
                       , profTickSpeed   = read tickSpeed
                       , profNbCpus      = read nbCpus
                       , profTotalAlloc  = Bytes $ readCommaNb nbBytes
                       }
    parseHeader (hdr) =
        error ("header format not expected: " ++ show hdr)
    readCommaNb = read . filter (/= ',')

    parseToplevels = map toSummary . drop 1

    parseCC = map toLine . drop 1

    splitSection = break (isPrefixOf "COST CENTRE")
    toLine l = case F.parse parseLine l of
                    F.ParseFail e   -> error ("parse error: " ++ show e)
                    F.ParseMore k   -> case k Nothing of
                                            F.ParseFail e   -> error ("parse error: " ++ show e ++ "\n" ++ show l)
                                            F.ParseOK inp a -> a
                    F.ParseOK inp a ->
                        case inp of
                            "" -> a
                            _  -> error ("holding on some more input:\n" ++ show inp ++ "\n" ++ show l)

    parseLine =
        ProfLine <$> (length <$> F.takeWhile isSpace)
                 <*> F.takeWhile (not . isSpace)
                 <*> (skipSpace *> F.takeWhile (not . isSpace))
                 <*> (skipSpace *> parseLocation)
                 <*> (skipSpace *> parseNb)
                 <*> (skipSpace *> parseNb)
                 <*> (skipSpace *> readPercent)
                 <*> (skipSpace *> readPercent)
                 <*> (skipSpace *> readPercent)
                 <*> (skipSpace *> readPercentEnd)
      where
        skipSpace = F.skipWhile isSpace
        readPercent :: F.Parser [Char] Percent
        readPercent = Percent . read <$> F.takeWhile (\c -> isDigit c || c == '.')
        readPercentEnd = Percent . read <$> F.takeAll
        parseNb = read <$> F.takeWhile isDigit
        parseLocation = (NoLoc <$> parseSpecial)
                    <|> (Loc <$> parseFile <*> parseLines)

        parseSpecial = F.element '<' *> F.takeWhile (/= '>') <* F.element '>'
        parseFile = F.takeWhile (/= ':')
        parseLines = F.takeWhile (not . isSpace)

    toSummary l =
        case words l of
            [f1,f2,f3,f4] -> ProfLineSummary f1 f2 (Percent $ read f3) (Percent $ read f4)
            _             -> error ("summary: number of field (expecting 4): " ++ show l)

showFmt :: Show t => Int -> t -> String
showFmt n v = replicate (n - len) ' ' ++ s
  where
    len = length s
    s = show v

printProfLine :: ProfLine -> String
printProfLine (ProfLine _ cost _modul _loc _no _entries _indTime _indAlloc inhTime inhAlloc) =
    (showFmt 5 inhTime ++ "  " ++ showFmt 5 inhAlloc ++ "  | " ++ cost )

diffProf :: Prof -> Prof -> ProfDiff
diffProf (Prof ph1 _ _) (Prof ph2 _ _) =
    ProfDiff { profDiffTotalTime = (makeDiff unSeconds `on` profTotalTime) ph1 ph2
             , profDiffTicks     = (makeDiff fromIntegral `on` profNbTicks) ph1 ph2
             , profDiffAlloc     = (makeDiff (fromIntegral . unBytes) `on` profTotalAlloc) ph1 ph2
             }
  where
    makeDiff :: Num a => (a -> Double) -> a -> a -> Diff a
    makeDiff toD a b = Diff a (b - a) (Percent $ (toD (b * 100) / toD a) - 100)

statsProfs :: [Prof] -> ProfStats
statsProfs profs = ProfStats
    { profStatsTime  = stats $ map profTotalTime hdrs
    -- , profStatsTicks = average $ map profNbTicks hdrs
    , profStatsAlloc = stats $ map profTotalAlloc hdrs
    }
  where
    hdrs = map profHeader profs
