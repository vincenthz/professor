{-# LANGUAGE OverloadedStrings #-}
module UI where

import Data.Monoid ((<>))

import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

import Brick.Types ( Widget , ViewportType(..) )
import Brick.Widgets.Core ( vLimit , vBox , viewport , str , withAttr , (<+>) )
import qualified Brick.Widgets.List as W

import qualified Brick.AttrMap as A

import Professor.ProfFile

import Brick.Util (fg, on)
import Utils
import Types

data Global = Global
    { globalThreshold :: !Percent
    , globalCostMode  :: !CostMode
    } deriving (Show,Eq)

data DisplayMode =
      DisplayOne [TreeProf]
    | DisplayDiff [TreeProfDiff]

data ListNode a = ListNode Expanded a

data ListContent = ListOne  TreeProf
                 | ListDiff TreeProfDiff

data Expanded = UnExpanded | Expanded | NoExpand
    deriving (Show,Eq)

data Widgets = Widgets
    { stList       :: W.List Name (ListNode ListContent)
    , stBar        :: !Bar
    , stBottomInfo :: !String
    }

data St = St
    { stTreeProf  :: DisplayMode
    , stWidgets   :: Widgets
    , stGlobal    :: Global
    , stContext   :: [St]
    }

data Name = TOPBAR | BOTTOMBAR | VP | LIST
    deriving (Ord, Show, Eq)

drawUi :: St -> [Widget Name]
drawUi st = [ui]
  where
    widgets = stWidgets st
    glob = stGlobal st

    ui = C.center $ B.border $ vBox [ topBar, B.hBorder, listProfile, B.hBorder, bottomBar ]
    topBar = withAttr titleAttr $ str $ renderBar $ stBar widgets

    bottomBar = withAttr titleAttr $ str $ stBottomInfo widgets

    listProfile = viewport VP Vertical
                $ vLimit 40
                $ W.renderList (listDrawElement glob) True
                $ stList widgets

    renderBar :: Bar -> String
    renderBar (Bar ctxTime totalTime alloc totalAlloc label) =
           (if globalCostMode glob == Inherited then "Inherited  | " else "Individual | ")
        ++ show (globalThreshold glob) ++ " | "
        ++ show ctxTime ++ " (" ++ show timePercent ++ ")"
        ++ "   " ++ show alloc ++ " "
        ++ "(" ++ show allocPercent ++ ")"
        ++ "   " ++ label
      where
        timePercent, allocPercent :: Percent
        timePercent = makePercent ctxTime totalTime
        allocPercent = makePercent (fromIntegral alloc) (fromIntegral totalAlloc :: Integer)

barReduce :: ProfLine -> Bar -> Bar
barReduce pl (Bar _ totalTime _ totalAlloc _) =
    Bar cTime totalTime cAlloc totalAlloc label
  where
    label = plModule pl ++ " " ++ strip (plCostCenter pl) ++ "  " ++ show (plEntries pl)
    cTime :: Seconds
    cTime = plInheritedTime pl `perSeconds` totalTime

    cAlloc :: Bytes
    cAlloc = plInheritedAlloc pl `perAlloc` totalAlloc

customAttr :: A.AttrName
customAttr = W.listSelectedAttr <> "custom"

importantAttr :: A.AttrName
importantAttr = W.listAttr <> "important"

importantSelAttr :: A.AttrName
importantSelAttr = W.listSelectedAttr <> "importantSel"

greenAttr :: A.AttrName
greenAttr = "green"

redAttr :: A.AttrName
redAttr = "red"

yellowAttr :: A.AttrName
yellowAttr = "yellow"

titleAttr :: A.AttrName
titleAttr = "title"

isImportant :: Global -> ProfLine -> Bool
isImportant glob pl
    | cm == Inherited = (plInheritedAlloc pl > thres || plInheritedTime pl > thres)
    | otherwise       = (plIndividualAlloc pl > thres || plIndividualTime pl > thres)
  where
    cm = globalCostMode glob
    thres = globalThreshold glob

isNotZero :: Global -> ProfLine -> Bool
isNotZero glob pl
    | cm == Inherited = (plInheritedAlloc pl > 0.001 || plInheritedTime pl > 0.001)
    | otherwise       = (plIndividualAlloc pl > 0.001 || plIndividualTime pl > 0.001)
  where
    cm = globalCostMode glob

--app :: M.App St e Name
app appEvent = M.App
    { M.appDraw = drawUi
    , M.appStartEvent = return
    , M.appHandleEvent = appEvent
    , M.appAttrMap = const $ A.attrMap V.defAttr
        [ (W.listAttr, fg V.white)
        , (W.listSelectedAttr, V.blue `on` V.white)
        , (customAttr, fg V.cyan)
        , (importantAttr, V.white `on` V.red)
        , (importantSelAttr, fg V.red)
        , (greenAttr, fg V.green)
        , (redAttr, fg V.red)
        , (yellowAttr, fg V.yellow)
        , (titleAttr, V.white `on` V.blue)
        ]
    , M.appChooseCursor = M.neverShowCursor
    }

showAbsPercent :: Global -> Percent -> Widget Name
showAbsPercent g v@(Percent f)
    | v > globalThreshold g = withAttr redAttr (str $ showDouble f)
    | v > 0.01              = withAttr yellowAttr (str $ showDouble f)
    | otherwise             = str $ showDouble f

showPositiveNegative f
    | f < (-0.01) = withAttr greenAttr (str $ showDouble f)
    | f > 0.01    = withAttr redAttr (str $ showDouble f)
    | otherwise   = str $ showDouble f

listDrawElement :: Global -> Bool -> ListNode ListContent -> Widget Name
listDrawElement glob sel (ListNode expanded (ListDiff lm)) =
    case lm of
        TreeDiffMatch l r _ ->
            let d = profLineDiff l r

                absAttr
                    | isImportant glob l || isImportant glob r = withAttr redAttr
                    | otherwise                                = id

                plTime | globalCostMode glob == Inherited = plInheritedTime l
                       | otherwise                        = plIndividualTime l
                plAlloc | globalCostMode glob == Inherited = plInheritedAlloc l
                        | otherwise                        =  plIndividualAlloc l

                pldTime | globalCostMode glob == Inherited = pldInheritedTime d
                        | otherwise                        = pldIndividualTime d
                pldAlloc | globalCostMode glob == Inherited = pldInheritedAlloc d
                         | otherwise                        = pldIndividualAlloc d
                cc = replicate (plHeadingSpace l) ' ' ++ charExp expanded ++ " " ++ fqCC l
                s =
                    (if sel then (withAttr W.listSelectedAttr) else id)
                        (
                        showAbsPercent glob plTime
                    <+> str " "
                    <+> showAbsPercent glob plAlloc
                    <+> str " "
                    <+> str (showFmt 10 (plEntries l) ++ " ")
                    <+> str " "
                    <+> showPositiveNegative (unPercent pldTime)
                    <+> str " "
                    <+> showPositiveNegative (unPercent pldAlloc)
                    <+> (str (" " ++ showFmt 7 (pldEntries d) ++ " "))
                    <+> (str cc)
                        )
             in s
        TreeDiffNotMatch (Left l) ->
            str $ "    RIGHT=" ++ fqCC (treeProfGetLine l)
        TreeDiffNotMatch (Right r) ->
            str $ "    LEFT=" ++ fqCC (treeProfGetLine r)
listDrawElement glob sel (ListNode expanded (ListOne (TreeProf a _ _))) =
    (if sel then (withAttr W.listSelectedAttr) else id) s
  where
    absAttr
        | isImportant glob a = withAttr redAttr
        | isNotZero glob a   = withAttr yellowAttr
        | otherwise          = id
    s =     absAttr (str $ showDouble (unPercent plTime))
        <+> str " "
        <+> absAttr (str $ showDouble (unPercent plAlloc))
        <+> str (" " ++ showFmt 9 (plEntries a))
        <+> str (" " ++ cc ++ spacing costCenter lenCC ++ plModule a)

    cc = replicate (plHeadingSpace a) ' ' ++ charExp expanded ++ " " ++ plCostCenter a

    plTime | globalCostMode glob == Inherited = plInheritedTime a
           | otherwise                        = plIndividualTime a
    plAlloc | globalCostMode glob == Inherited = plInheritedAlloc a
            | otherwise                        =  plIndividualAlloc a

    spacing n m = replicate (n - m) ' '

    costCenter = 100
    lenCC = length cc

charExp :: Expanded -> String
charExp Expanded   = "-"
charExp UnExpanded = "+"
charExp NoExpand   = "|"
