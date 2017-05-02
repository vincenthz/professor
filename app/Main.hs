{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative

import Control.Monad (void, foldM, forM_, forM, filterM)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M


import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  , withAttr
  , (<+>)
  )
import qualified Brick.Widgets.List as W

import Data.List
import qualified Data.Function as F
import System.Environment
import System.Directory
import System.FilePath
import System.Exit

import qualified Data.Vector as V
import Brick.Util (fg, on)
import Text.Printf
import Utils
import UI
import Types
import Professor.ProfFile

popContext st = case stContext st of
                    []              -> st
                    parentContext:_ -> parentContext

pushContext st =
    case (stTreeProf st, W.listSelectedElement (stList $ stWidgets st)) of
        (_, Nothing) -> st
        (DisplayDiff _   , Just (_, ListNode _ (ListDiff _))) -> st
        (DisplayOne trees, Just (_, ListNode _ (ListOne (TreeProf x _ children)))) ->
                let innerList = W.list LIST (V.fromList $ treeExpandABit children) 1
                    innerBar = barReduce x (stBar $ stWidgets st)
                 in St { stTreeProf  = DisplayOne children
                       , stWidgets   = Widgets innerList innerBar ""
                       , stGlobal    = stGlobal st
                       , stContext   = st : stContext st
                       }
        _ -> error "mode mismatch"

listContentExpand (ListNode _ x) = ListNode Expanded x

listContentUnexpand (ListNode _ x) = ListNode UnExpanded x

expandCurrent st = return $
    case (stTreeProf st, W.listSelectedElement (stList $ stWidgets st)) of
        (_                , Nothing)                                 -> st
        (DisplayOne trees , Just (listIndex, (ListNode expanded (ListOne e))))  -> doOne trees listIndex expanded e
        (DisplayDiff trees, Just (listIndex, (ListNode expanded (ListDiff e)))) -> doDiff trees listIndex expanded e
        _                                                            -> error "internal error: invalid mode"
  where
    doOne _     _         Expanded   _                     = st
    doOne _     _         NoExpand   _                     = st
    doOne trees listIndex UnExpanded (TreeProf _ _ children) =
        let origList = W.listModify listContentExpand $ stList $ stWidgets st
            newList  = addList (listIndex+1)
                               (map (\c -> ListNode (expandableTree c) (ListOne c)) children)
                               origList
         in modifyWidgetList st (const newList)

    doDiff _     _         Expanded   _ = st
    doDiff _     _         NoExpand   _ = st
    doDiff trees listIndex UnExpanded (TreeDiffNotMatch {})        = st
    doDiff trees listIndex UnExpanded (TreeDiffMatch _ _ children) =
        let origList = W.listModify listContentExpand $ stList $ stWidgets st

            newList = addList (listIndex+1)
                              (map (\c -> ListNode (expandableDiff c) (ListDiff c)) children)
                              origList
         in modifyWidgetList st (const newList)

    addList :: Int -> [element] -> W.List name element -> W.List name element
    addList startIndex elements origList =
        foldl (\list (i, e) -> W.listInsert i e list) origList (zip [startIndex..] elements)

expandableTree (TreeProf _ _ []) = NoExpand
expandableTree (TreeProf _ _ _)  = UnExpanded

expandableDiff (TreeDiffMatch _ _ []) = NoExpand
expandableDiff (TreeDiffMatch _ _ _)  = UnExpanded
expandableDiff (TreeDiffNotMatch _)   = NoExpand

unexpandCurrent st = return $
    case W.listSelectedElement (stList $ stWidgets st) of
        Just (listIndex, ListNode Expanded (ListOne e)) ->
            let origList = W.listMoveDown $ W.listModify listContentUnexpand $ stList $ stWidgets st
                lvl = plHeadingSpace $ treeProfGetLine e
                newList = removeAllChildren lvl origList
            in modifyWidgetList st (const newList)
        _                          -> st
        --Just (_        , (Expanded, _)) -> st -- TODO un-expand
        --Just (listIndex, (NoExpand, e)) -> st
  where
    removeAllChildren lvl list =
        case W.listSelectedElement list of
            Nothing     -> list
            Just (idx, (ListNode _ (ListOne t)))
                | plHeadingSpace (treeProfGetLine t) > lvl -> removeAllChildren lvl (W.listMoveDown $ W.listRemove idx list)
                | otherwise                                -> W.listMoveUp list

appEvent :: St
         -> T.BrickEvent Name e
         -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev@(V.EvKey k q)) =
    case (k, q) of
        (V.KEsc, [])      -> M.halt st
        (V.KDown, [])     -> M.continue $ modifyWidgetList st W.listMoveDown
        (V.KUp,   [])     -> M.continue $ modifyWidgetList st W.listMoveUp
        (V.KPageDown, []) -> M.continue $ modifyWidgetList st (W.listMoveBy 80)
        (V.KPageUp, [])   -> M.continue $ modifyWidgetList st (W.listMoveBy (-80))
        (V.KChar 'i', []) -> M.continue (modifyGlob st $ \glob -> glob { globalCostMode = if globalCostMode glob == Inherited then Individual else Inherited })

        (V.KChar '-', []) -> M.continue (modifyGlob st $ \glob -> glob { globalThreshold = max 0 (globalThreshold glob - 1) })
        (V.KChar '+', []) -> M.continue (modifyGlob st $ \glob -> glob { globalThreshold = min 99 (globalThreshold glob + 1) })
        (V.KChar '[', []) -> M.continue (popContext st)
        (V.KChar ']', []) -> M.continue (pushContext st)
        (V.KRight, [])    -> expandCurrent st >>= M.continue
        (V.KLeft, [])     -> unexpandCurrent st >>= M.continue
        (_,       _)      -> W.handleListEvent ev (stList $ stWidgets st) >> M.continue st
appEvent st _ = M.continue st

modifyGlob st f = st { stGlobal = f (stGlobal st) }
modifyWidgets st f = st { stWidgets = f (stWidgets st) }

modifyWidgetList st f = modifyWidgets st $ \widgets ->
    let newList = f $ stList widgets
        newInfo = case (stTreeProf st, W.listSelectedElement newList) of
                    (_, Nothing)      -> ""
                    (DisplayOne treeProfs, Just (listIndex, ListNode _ (ListOne (TreeProf e p _)))) ->
                        "loc: " ++ show (plLocation e) ++ "  " ++
                        maybe "" (\pl -> "parent: " ++ fqCC pl
                                      ++ " time: " ++ showDouble (unPercent $ plInheritedTime pl)
                                      ++ " alloc: " ++ showDouble (unPercent $ plInheritedAlloc pl)
                                 ) p
                    (DisplayDiff _, Just (listIndex, ListNode _ (ListDiff e))) -> "diff mode ..."
     in widgets { stList = newList, stBottomInfo = newInfo }

treePrint indent depth (TreeProf profLine _ children)
    | depth == 0 = putStrLn (replicate indent ' ' ++ plCostCenter profLine)
    | otherwise  = putStrLn (replicate indent ' ' ++ plCostCenter profLine)
                >> mapM_ (treePrint (indent + 2) (depth-1)) children

startThreshold :: Percent
startThreshold = Percent 7.9

startGlob = Global startThreshold Inherited

treeExpandABit :: [TreeProf] -> [ListNode ListContent]
treeExpandABit = loop
  where
    loop [] = []
    loop (x:xs)
        | isImportant startGlob (treeProfGetLine x) = ListNode Expanded (ListOne x) : loop (treeProfGetChildren x) ++ loop xs
        | otherwise                                 = ListNode (expandableTree x) (ListOne x) : loop xs

treeDiffExpandABit :: [TreeProfDiff] -> [ListNode ListContent]
treeDiffExpandABit = map (ListNode UnExpanded . ListDiff)

readProf file = parseProf <$> readFile file
filterProfs = filter (isSuffixOf ".prof")

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            prof <- readProf file

            let glob = Global startThreshold Inherited

            let tprof = treeParseProf Nothing $ profCostCenterLines prof
                li    = W.list LIST (V.fromList $ treeExpandABit tprof) 1

            putStrLn $ show $ profHeader prof
            putStrLn $ show (length $ profCostCenterLines prof) ++ " records"

            let hdr = profHeader prof
                b = Bar (profTotalTime hdr) (profTotalTime hdr) (profTotalAlloc hdr) (profTotalAlloc hdr) "MAIN"
            let st = St (DisplayOne tprof) (Widgets li b "") glob []
            void $ M.defaultMain (app appEvent) st
        [file1,file2] -> do
            profLeft  <- readProf file1
            profRight <- readProf file2

            let glob = Global startThreshold Inherited

            let tprofLeft  = treeParseProf Nothing $ profCostCenterLines profLeft
                tprofRight = treeParseProf Nothing $ profCostCenterLines profRight
                tdiff      = treeProfDiff tprofLeft tprofRight
                li         = W.list LIST (V.fromList $ treeDiffExpandABit tdiff) 1

            let b = Bar 0 0 0 0 "DIFF" -- (profTotalTime hdr) (profTotalTime hdr) (profTotalAlloc hdr) (profTotalAlloc hdr) "MAIN"
            let st = St (DisplayDiff tdiff) (Widgets li b "") glob []
            void $ M.defaultMain (app appEvent) st
        _ -> do
            putStrLn "error: professor <file.prof> [file2.prof]"
            exitFailure
