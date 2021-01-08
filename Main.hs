{-
    InGraph - Ingress link optimizer

    Copyright (C) 2013  Nigel D. Stepp

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

    Nigel Stepp <stepp@atistar.net>
    http://www.atistar.net/~stepp/ingraph/

    $Id: Main.hs 861 2014-08-18 17:34:13Z stepp $
-}

-- |InGraph, an ingress graph optimizer
module Main where

import InGraph
import Paths_ingraph
import DataDict
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Graph.Inductive hiding (size)
import System.FilePath
import System.Directory
import System.Environment
import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Monad
import Control.Monad.Random.Strict
import Control.Exception


version = "0.11"


aboutTxt =
    "InGraph " ++ Main.version ++ "\n"
    ++ "Ingress portal link optimizer\n\n"
    ++ "Copyright © 2013 Nigel Stepp\n"
    ++ "<stepp@atistar.net>"


-- | Look for the path to the application icon.
-- If it doesn't exist, return Nothing
getIconPath :: IO (Maybe FilePath)
getIconPath =
    let paths = [ getDataFileName "",
                  getAppUserDataDirectory "ingraph",
                  (liftM dropFileName) getExecutablePath,
                  getCurrentDirectory
                  ]
    in findFileM paths "ingraph-icon.png"
    where
        findFileM fp s = join $ liftM2 findFile (sequence fp) (return s)

main :: IO ()
main = start doFrame



-- | Specify image files for the file open dialog.
imageFiles
   = [("Image files",["*.bmp","*.jpg","*.gif","*.png"])
     ,("Portable Network Graphics (*.png)",["*.png"])
     ,("BMP files (*.bmp)",["*.bmp"])
     ,("JPG files (*.jpg)",["*.jpg"])
     ,("GIF files (*.gif)",["*.gif"])
     ]

-- | Graph file types for graph load and save dialogs.
graphFiles
    = [("Graph files (*.gph)",["*.gph"])]

-- | GUI entry point. Set up window elements and define callbacks
doFrame :: IO ()
doFrame
    = do f <- frame [ text := "InGraph: Ingress Link Optimizer" ]

         -- Check for the application icon
         iconPath <- getIconPath

         -- Set the icon if it exists, otherwise
         -- don't bother the user about it
         case iconPath of
            Just p -> set f [ picture := p ]
            Nothing -> return ()
            --Nothing -> warningDialog f "Warning" "Cannot find icon, Ingraph may not be fully installed"

         -- Mutable variables
         varPortalMap <- variable [ value := Nothing ]
         varPortalGraph <- variable [ value := emptyGraph ]
         varOptFunc <- variable [ value := 0 ]
         varOptMode <- variable [ value := LinkMode ]


         -- Window where we draw the portal graph
         w <- scrolledWindow f [ scrollRate := sz 10 10,
                                 bgcolor := white,
                                 fullRepaintOnResize := False ]


         -- Menu items
         mFile <- menuPane [ text := "&File" ]
         mOpen <- menuItem mFile [ text := "&Open Image..." ]
         mLoad <- menuItem mFile [ text := "&Load Graph..." ]
         mSave <- menuItem mFile [ text := "&Save Graph..."]
         mClose <- menuItem mFile [ text := "&Close" ]
         menuLine mFile
         mQuit <- menuQuit mFile [ help := "Quit" ]

         mView <- menuPane [ text := "&View" ]
         mRank <- menuItem mView [ text := "Portal Ranking..." ]
         mLinks <- menuItem mView [ text := "Links..." ]

         mHelp <- menuHelp []
         mAbout <- menuAbout mHelp [ help := "About InGraph" ]


         status <- statusField [ text := "Open a portal map image to begin" ]

         -- Panel for controls and info text
         p <- panel f []
         gainSlider <- hslider p True 0 100 [ selection := 2,
            tooltip := "If there is a choice, how important are fields?" ]

         eapText <- staticText p [ text := "      " ]
         fapText <- staticText p [ text := "      " ]
         ratioText <- staticText p [ text := "     " ]
         fieldText <- staticText p [ text := "   " ]
         linkText <- staticText p [ text := "   " ]

         optButton <- button p [ text := "Optimize!", enabled := False ]
         resetButton <- button p [ text := "Reset" ]

         optIterations <- spinCtrl p 100 5000 [ selection := 500,
            tooltip := "More iterations has a better chance of finding an optimal configuration" ]

         let optFuncs = ["AP Ratio", "AP", "Links", "Fields", "Same # Links", "Save Flynn Event","Link Defense","Min Keys"]
         optFunc <- choice p [ items := optFuncs, selection := 0 ]

         let optModes = ["Links", "Fields"]
         optMode <- choice p [ items := optModes, selection := 0 ]

         -- Store items in a data dictionary
         {-
         let dataDict = DataDict {
                buttonDict = Dict [(ButtonKey OptButton, optButton),
                                   (ButtonKey ResetButton, resetButton)],
                textDict = Dict [(TextKey EAPText, eapText),
                                 (TextKey FAPText, fapText),
                                 (TextKey RatioText, ratioText),
                                 (TextKey FieldText, fieldText),
                                 (TextKey LinkText, linkText)],
                controlDict = Dict [(ControlKey GainSlider, gainSlider)],
                spinDict = Dict [(SpinKey IterationSpin, optIterations)]
                }
         -}

         let widgets = WidgetConfig {
            cfgButtons = Map.fromList [
                (OptButton,optButton),
                (ResetButton,resetButton) ],
            cfgText = Map.fromList [
                (EAPText, eapText),
                (FAPText, fapText),
                (RatioText, ratioText),
                (FieldText, fieldText),
                (LinkText, linkText) ],
            cfgSliders = Map.fromList [(GainSlider, gainSlider)],
            cfgSpinners = Map.fromList [(IterationSpin, optIterations)]
            }

         set w [ on click := addPortal f w varPortalGraph widgets,
                 on paint := onPaint varPortalMap varPortalGraph ]

         set optButton [ on command := optimizeLinks w varPortalGraph varOptFunc varOptMode widgets ]
         set resetButton [ on command := clearGraph w varPortalGraph widgets ]

         set optFunc [ on select ::= updateOptFunc varOptFunc ]
         set optMode [ on select ::= updateOptMode varOptMode ]

         -- Lay out the frame using the bits defined above
         set f [ layout := fill $ row 5 [
                               fill (widget w),
                               vfill $ boxed "Tools" $ container p $ column 5 [
                                   label "Optimization Function:",
                                   hfill $ widget optFunc,
                                   label "Optimize using:",
                                   hfill $ widget optMode,
                                   row 2 [ label "Iterations: ", widget optIterations],
                                   row 2 [ widget optButton, widget resetButton ],
                                   label "Field Gain (%):",
                                   hfill $ widget gainSlider,
                                   row 2 [label "Enemy AP: ", hfill $ widget eapText],
                                   row 2 [label "Friendly AP: ", hfill $ widget fapText],
                                   label "Ratio (lower is better):",
                                   hfill $ widget ratioText,
                                   row 2 [label "Fields: ",
                                          hfill $ widget fieldText],
                                   row 2 [label "Links: ",
                                          hfill $ widget linkText]
                                   ]
                               ],
                 statusBar := [status],
                 menuBar := [mFile, mView, mHelp],
                 outerSize := sz 800 600,
                 on (menu mQuit) := close f,
                 on (menu mOpen) := onOpen f w varPortalMap varPortalGraph mClose status,
                 --on (menu mSave) := savePortalImage varPortalMap varPortalGraph,
                 on (menu mLoad) := loadPortalGraph f w varPortalGraph widgets,
                 on (menu mSave) := savePortalGraph f varPortalGraph,
                 on (menu mClose) := onClose w varPortalMap mClose status,
                 on (menu mRank) := showRankings varPortalGraph,
                 on (menu mLinks) := showLinks varPortalGraph,
                 on (menu mAbout)  := infoDialog f "About InGraph" aboutTxt
                 ]


onOpen :: Frame a -> ScrolledWindow b -> Var (Maybe (Bitmap ())) -> Var PortalGraph -> MenuItem c -> StatusField -> IO ()
onOpen f w varPortalMap varPortalGraph mClose status
    = do imageFile <- fileOpenDialog f True True "Open image" imageFiles "" ""
         case imageFile of
            Nothing -> return ()
            Just file -> openImage w varPortalMap mClose status file
         varSet varPortalGraph emptyGraph

onClose w varPortalMap mClose status
    = do repaint w

openImage w varPortalMap mClose status file
    = do bm <- bitmapCreateFromFile file
         set varPortalMap [ value := Just bm ]
         set status [ text := file ]
         bmsize <- get bm size
         set w [ virtualSize := bmsize ]
         repaint w
      `onException` repaint w

onPaint :: Var (Maybe (Bitmap ())) -> Var PortalGraph -> DC e -> Rect -> IO ()
onPaint varPortalMap varPortalGraph dc viewArea
    = do bmap <- get varPortalMap value
         case bmap of
            Nothing -> return ()
            Just bm -> drawBitmap dc bm pointZero False []
         drawPortalGraph dc varPortalGraph

-- |Given a graph, draw the nodes and edges
drawPortalGraph :: DC a -> Var PortalGraph -> IO ()
drawPortalGraph dc varPortalGraph = do
    g <- get varPortalGraph value
    let portals = map snd (labNodes g)
        links = map (mapTuple (fromJust.lab g)) (edges g)
    drawPortalLinks dc (map (mapTuple (mapSnd vec2point)) links)
    drawPortals dc (map (mapSnd vec2point) portals)
    drawLabels dc (map (mapSnd vec2point) portals)

-- |Draw links between portals
drawPortalLinks :: DC a -> [((String,Point),(String,Point))] -> IO ()
drawPortalLinks dc [] = return ()
drawPortalLinks dc (l:ls) = do
    let portal1 = fst l
        portal2 = snd l
        pos1 = snd portal1
        pos2 = snd portal2
    set dc [pen := penColored red 3]
    line dc pos1 pos2 []
    drawPortalLinks dc ls

-- |Draw a dot for each portal in the graph
drawPortals :: DC a -> [(String,Point)] -> IO ()
drawPortals dc [] = return ()
drawPortals dc (p:ps)
    = do
        let portalName = fst p
            portalPos = snd p
        set dc [brushColor := red, brushKind := BrushSolid]
        circle dc portalPos 5 []
        drawPortals dc ps

-- |Draw each portal label
drawLabels :: DC a -> [(String,Point)] -> IO ()
drawLabels dc [] = return ()
drawLabels dc (p:ps)
    = do
        let portalName = fst p
            portalPos = snd p
            portalTextPos = pointShift portalPos (5,5)
        drawText dc portalName portalTextPos [textColor := white, fontFamily := FontSwiss]
        drawLabels dc ps
        where
            pointShift (Point x y) (dx,dy) = Point (x+dx) (y+dy)

-- |Write out a text representation of the graph to a file.
-- The file format is compatible with the GraphViz \"dot\" file format, except for the pos attribute.
-- GraphViz will still display the graph topology, however.
savePortalGraph f varPortalGraph
    = do
        g <- get varPortalGraph value
        graphFile <- fileSaveDialog f True True "Save graph" graphFiles "" ""
        let nodeStr = concatMap (\n->"N" ++ show n
                ++ "[label=\"" ++ (fst $ fromJust $ lab g n)
                ++ "\", pos=\"("
                ++ (tail.init) (show $ snd $ fromJust $ lab g n)
                ++ ")\"]\n") (nodes g)
            edgeStr = concatMap (\(f,t)->"N" ++ show f
                ++ " -> N" ++ show t ++ "\n") (edges g)
        case graphFile of
            Nothing -> return ()
            Just file -> writeFile file ("digraph PortalLinks {\n" ++ nodeStr ++ edgeStr ++ "}\n")

-- |Read in the graph from a text representation.
-- See 'savePortalGraph' for comments about the file format. As it is now, this routine
-- expects the pos attribute to be a tuple \"(x,y)\" instead of the usual \"x,y!\" from the
-- dot-file specification. This should be remedied in future versions.
loadPortalGraph f w varPortalGraph widgets
    = do
        graphFile <- fileOpenDialog f True True "Open graph" graphFiles "" ""
        case graphFile of
            Nothing -> return ()
            Just file -> do
                dotStrs <- (liftM lines) $ readFile file
                let nodeStrs = filter (isJust . elemIndex '[') dotStrs
                    edgeStrs = filter (isInfixOf "->") dotStrs
                    tempStrs = map (\s->map (\n->drop (n+2) s) (elemIndices '=' s)) nodeStrs
                    labels = transpose $ map (map (\s->take (fromJust$findIndex (=='"') s) s)) tempStrs
                    nodeLabels = head labels
                    nodeVecs = map (Vec . read) (last labels)::[InGraph.Vector Float]
                    portals = zip nodeLabels nodeVecs
                    edges = map (\s->(read $ drop 1 $ head (words s)::Int, read $ drop 1 $ last (words s)::Int,())) edgeStrs
                    g = insEdges edges $ addPortals emptyGraph portals
                    optButton = getCfg widgets OptButton
                set varPortalGraph [ value := g ]
                set optButton [ enabled := True ]
                repaint w


-- | Save the current bitmap to a file.
-- NOTE! This is currently not functional, as the necessary wxWidget routines are not implemented in wxHaskell.
savePortalImage varPortalMap varPortalGraph
    = do
        bmap <- get varPortalMap value
        outBmap <- bitmapCreateEmpty (sz 400 400) 8
        dc <- memoryDCCreate
        memoryDCSelectObject dc outBmap
        case bmap of
            Nothing -> return ()
            Just bm -> drawBitmap dc bm pointZero False []
        drawPortalGraph dc varPortalGraph
        img <- imageCreateFromBitmap outBmap
        imageSaveFile img "ingraphout.png" (imageTypeFromExtension "png")
        --pal <- paletteCreateDefault
        --bitmapSaveFile outBmap "ingraphout.png" (imageTypeFromExtension "png") pal
        return ()


-- |Given a click location, add a new portal to the graph.
-- A modal dialog is presented so that the user can enter a label for the new portal.
addPortal :: Frame a -> ScrolledWindow b -> Var PortalGraph -> WidgetConfig -> Point -> IO ()
addPortal f w varPortalGraph widgets pt
    = do graph <- varGet varPortalGraph
         let defaultName = "portal" ++ show (noNodes graph)
         portalName <- textDialog f "Please enter a name for this portal" "Name" defaultName
         let optButton = getCfg widgets OptButton
         if not (null portalName)
             then do set optButton [ enabled := True ]
                     varUpdate varPortalGraph (\g -> addPortals g [(portalName,point2vec pt)] )
             else varGet varPortalGraph
         repaint w

-- |Utility function to map a function to both elements
-- of a tuple
mapTuple :: (a -> b) -> ( (a,a) -> (b,b) )
mapTuple f = f >< f

-- |Convert a wxHaskell Point to a Vector used by the
-- graph routines
point2vec :: Point -> InGraph.Vector Float
point2vec (Point x y) = Vec (fromIntegral x, fromIntegral y)

-- |Convert a Vector to a wxHaskell Point
vec2point :: InGraph.Vector Float -> Point
vec2point (Vec (x,y)) = Point (truncate x) (truncate y)

-- |Reset the graph state to the empty graph and clear all stats.
clearGraph w varPortalGraph widgets
    = do let optButton = getCfg widgets OptButton
         varSet varPortalGraph emptyGraph
         set (getCfg widgets EAPText) [ text := "" ]
         set (getCfg widgets FAPText) [ text := "" ]
         set (getCfg widgets RatioText) [ text := "" ]
         set (getCfg widgets FieldText) [ text := "" ]
         set (getCfg widgets LinkText) [ text := "" ]
         set optButton [ enabled := False ]
         repaint w

-- |Set the optimization function from the dropdown list
updateOptFunc varOptFunc selectedItem
    = do idx <- get selectedItem selection
         varSet varOptFunc idx
         return ()

-- |Set the optimization mode from the dropdown list
updateOptMode varOptMode selectedItem
    = do idx <- get selectedItem selection
         varSet varOptMode (case idx of
            0 -> LinkMode
            1 -> FieldMode)
         return ()

-- | Call "InGraph" optimization routine on the currently
-- defined graph.
-- The user selects which optimization function to use. See "InGraph#hamiltonians"
optimizeLinks w varPortalGraph varOptFunc varOptMode widgets
    = do let gainSlider = getCfg widgets GainSlider
             iterSpin = getCfg widgets IterationSpin
             eapText = getCfg widgets EAPText
             fapText = getCfg widgets FAPText
             ratioText = getCfg widgets RatioText
             fieldText = getCfg widgets FieldText
             linkText = getCfg widgets LinkText
         fieldGain <- get gainSlider selection
         optIterations <- get iterSpin selection
         optMode <- varGet varOptMode
         optFuncIdx <- varGet varOptFunc
         let h = case optFuncIdx of
                      0 -> hamiltonianRatio
                      1 -> hamiltonianAP
                      2 -> hamiltonianLinks
                      3 -> hamiltonianFields
                      4 -> hamiltonianDegree
                      5 -> hamiltonianFlynn
                      6 -> hamiltonianLinkDefense
                      7 -> hamiltonianKeys
                      --3 -> hamiltonianUCLA
         varUpdate varPortalGraph stripEdges
         oldGraph <- varGet varPortalGraph
         let optimizedGraph = (`evalRand` mkStdGen 1) $ graphOptimizeWith h ((fromIntegral fieldGain)/100.0) optIterations optMode oldGraph
         varSet varPortalGraph optimizedGraph
         let (eap,fap,fields,links) = graphStats optimizedGraph
         set eapText [ text := show eap ]
         set fapText [ text := show fap ]
         set fieldText [ text := show fields ]
         set linkText [ text := show links ]
         set ratioText [ text := show ((fromIntegral eap)/(fromIntegral fap)) ]
         repaint w

-- |List portal links
showLinks varPortalGraph
    = do f <- frame [ text := "Portal Links" ]
         p <- panel f []
         linkList <- textCtrl p []
         okButton <- button p [ text := "Ok" ]
         graph <- varGet varPortalGraph

         let links = edges (undir graph)
             linkTos = map (map snd) (groupBy (\x y -> fst x == fst y) links)
             linkFroms = concatMap (nub . map fst) (groupBy (\x y -> fst x == fst y) links)
             fromStrs = map (\n->(fst$fromJust$lab graph n) ++ " (" ++ (show (deg graph n)) ++ " keys max) -> ") linkFroms
             toStrs = map (intercalate ",\n\t" . map (\n->fst$fromJust$lab graph n)) linkTos
             --linkStrs = map (\(f,t)->(fst $ fromJust $ lab graph f) ++ " -> " ++ (fst $ fromJust $ lab graph t)) (edges (undir graph))
             linkStrs = zipWith (++) fromStrs toStrs

         set linkList [ text := intercalate "\n" linkStrs ]

         set f [ layout := container p $ margin 10 $ column 5 [
                                    --hfill $ widget infoText,
                                    fill (widget linkList),
                                    hfill $ widget okButton
                                  ],
                 clientSize := sz 400 300
               ]
         set okButton [ on command := close f ]

-- |List portals in order of importance
showRankings varPortalGraph
    = do f <- frame [ text := "Portal Importance Ranking" ]
         p <- panel f []
         rankList <- listCtrl p [
            columns := [ ("Rank", AlignCentre, 60),
                         ("Name", AlignLeft, 320)
                         ]
            ]
         okButton <- button p [ text := "Ok" ]
         infoText <- staticText p [ text := "This is a list of portals ranked by how important they are,\n"
                                         ++ "either to protect if the portal is friendly, or to attack\n"
                                         ++ "if it isn't.\n\n"
                                         ++ "The most important portals are ranked the highest." ]

         graph <- varGet varPortalGraph
         let portals = rankPortals graph
             entries = [ [idx,label] | (idx,label) <- zip (map show [1..(length portals)]) (map fst portals) ]

         set rankList [ items := entries ]

         set f [ layout := container p $ margin 10 $ column 5 [
                                    hfill $ widget infoText,
                                    fill (widget rankList),
                                    hfill $ widget okButton
                                  ],
                 clientSize := sz 400 300
               ]
         set okButton [ on command := close f ]

