module Main where

import Control.Applicative

import Control.Monad (void, foldM, forM_, forM, filterM)
import Data.Monoid ((<>))

import Data.List
import qualified Data.Function as F
import System.Environment
import System.Directory
import System.FilePath
import System.Exit

import Text.Printf
import Utils
import Professor.ProfFile

readProf :: FilePath -> IO Prof
readProf file = parseProf <$> readFile file

filterProfs :: [FilePath] -> [FilePath]
filterProfs = filter (isSuffixOf ".prof")

main :: IO ()
main = do
    args <- getArgs
    case args of
        "stats":baseDirs -> do
            putStrLn "stats"
            forM_ baseDirs $ \baseDir -> do
                putStrLn $ show baseDir
                profFiles <- filterProfs <$> getDirectoryContents baseDir
                profs     <- mapM (readProf . (baseDir </>)) profFiles

                let st = statsProfs profs
                putStrLn $ "time  => " ++ show (profStatsTime st)
                putStrLn $ "alloc => " ++ show (profStatsAlloc st)
                putStrLn ""

        "report":baseDir:r -> do
            let chosenBase = case r of
                                baseBin:_ -> Just baseBin
                                _         -> Nothing

            sets <- getDirectoryContentsDirs baseDir

            allStL <- forM sets $ \setName -> do
                let setDir = baseDir </> setName
                bins <- getDirectoryContentsDirs setDir
                forM bins $ \binName -> do
                    let binDir = setDir </> binName
                    profFiles <- filterProfs <$> getDirectoryContents binDir
                    profs     <- mapM (readProf . (binDir </>)) profFiles

                    let st = statsProfs profs
                    return (binName, (setName, st))
            let allSt = concat allStL

            let for = flip map

            let cols = nub $ sort $ map fst allSt :: [String]
                sets = nub $ sort $ map (fst . snd) allSt :: [String]

                -- for calculating differences
                baseBin = head cols
                baseSt  = map (snd) $ filter ((==) baseBin . fst) allSt

                toRow :: (ProfStats -> ProfStats -> String) -> String -> [String]
                toRow render set =
                    let x = map (\(x, (_,st)) -> (x,st)) $ filter (\(_,(s,_)) -> s == set) allSt
                     in set : (for cols $ \c -> case (lookup c x, lookup set baseSt) of
                                                  (Just st, Just baseSt) -> render st baseSt
                                                  (_, _)                 -> "N/A")

                showDiff cst bst = showFmt 6 (makePercent cst bst - Percent 100)

            let cpuRows = for sets $ toRow (\st baseSt -> show (statAvg $ profStatsTime st)
                                                       ++ if st == baseSt then "" else " " ++ (showDiff `F.on` (statAvg . profStatsTime)) st baseSt)
                memRows = for sets $ toRow (\st baseSt -> show (statAvg $ profStatsAlloc st)
                                                       ++ if st == baseSt then "" else " " ++ (showDiff `F.on` (statAvg . profStatsAlloc)) st baseSt)

            putStrLn $ tableMarkdown "CPU" cols cpuRows

            putStrLn $ tableMarkdown "Alloc" cols memRows

        "diff":baseDir:runDir:[] -> do
            bases  <- getDirectoryContents baseDir
            checks <- getDirectoryContents runDir

            putStrLn "diffing"

            return ()
        _ -> do
            putStrLn "error: professor-cli <command> <subargs>"
            putStrLn ""
            putStrLn "  stats <baseDir>"
            putStrLn "  report <baseDir> [leftmost]"
            putStrLn "  diff <baseDir> <runDir>"
            exitFailure
