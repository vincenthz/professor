{-# LANGUAGE BangPatterns #-}
module Utils
    where

import Professor.ProfFile
import Data.List
import Text.Printf
import System.Directory
import System.FilePath
import Control.Monad

perSeconds :: Percent -> Seconds -> Seconds
perSeconds (Percent p) (Seconds s) = Seconds (s * p / 100)

perAlloc :: Percent -> Bytes -> Bytes
perAlloc (Percent p) (Bytes b) = Bytes $ truncate $ ((p * fromIntegral b) / 100)

stripLeading :: String -> String
stripLeading [] = []
stripLeading (x:xs)
    | x == ' '  = stripLeading xs
    | otherwise = x:xs

strip :: String -> String
strip = stripLeading . reverse . stripLeading . reverse

leftJustify :: Int -> String -> String
leftJustify n s = replicate (n - len) ' ' ++ s
  where !len = length s

rightJustify :: Int -> String -> String
rightJustify n s = s ++ replicate (n - len) ' '
  where !len = length s

showFmt :: Show a => Int -> a -> String
showFmt n v = replicate (n - len) ' ' ++ s
  where
    len = length s
    s = show v

showDouble :: Double -> String
showDouble d = leftJustify 6 (printf "%.02f" d)

showDoubleAbs :: Double -> String
showDoubleAbs d = leftJustify 7 (printf "%.02f" d)

tableMarkdown :: String -> [String] -> [[String]] -> String
tableMarkdown name cols rows =
    let hdr = "| " ++ intercalate " | " (padList (name : cols)) ++ " |\n"
        sep = "|-" ++ intercalate "-|-" (map (map (const '-')) (padList (name : cols))) ++ "-|\n"
     in hdr ++ sep ++ concatMap printRow (map padList rows)
  where
    printRow :: [String] -> String
    printRow l = "| " ++ intercalate " | " l ++ " |\n"

    getColN n = map (flip (!!) n) rows

    sizeCols :: [Int]
    sizeCols = map (\(i, c) -> maximum $ map length (c : getColN i)) $ zip [0..] (name : cols)

    padList l = zipWith pad sizeCols l

    pad :: Int -> String -> String
    pad sz s
        | sz <= len = s
        | otherwise = replicate leftPad ' ' ++ s ++ replicate rightPad ' '
      where
        len = length s
        (leftPad, r) = (sz - len) `divMod` 2
        rightPad = leftPad + r

-- | Get only the directory in a specific directory
getDirectoryContentsDirs :: FilePath -> IO [FilePath]
getDirectoryContentsDirs dir = do
    allFiles <- filter (not . flip elem [".",".."]) <$> getDirectoryContents dir
    filterM (doesDirectoryExist . (dir </>)) allFiles
