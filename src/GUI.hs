module GUI
       (startGUI
       )
       where

import Paths_LabelGenerator

import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent

import System.Process
import System.Directory

import GHC.Float

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Backend
import LatexCode

data GUI = MainMenu { wMainWindow :: Window,
                      bBackcover :: Button,
                      bAccessNumber :: Button,
                      bCalibration :: Button,
                      bQuit :: Button } |
           
           LabelCreation { wMainWindow :: Window,
                           subGUI :: SubGUI,
                           cbOffset :: CheckButton,
                           offsetRows :: SpinButton,
                           offsetColumns :: SpinButton,
                           bCancel :: Button,
                           bOK :: Button } |

           Calibration { wMainWindow:: Window,
                         sbTopOffset :: SpinButton,
                         sbBottomOffset :: SpinButton,
                         sbLeftOffset :: SpinButton,
                         sbRightOffset :: SpinButton,
                         bPrintTestPage :: Button,
                         bCalibCancel :: Button,
                         bCalibSave :: Button}

data SubGUI = Backcover { boundingBox :: VBox,
                          boxEntrySpace :: VBox,
                          entries :: [BackcoverEntry],
                          bAddLabel :: Button }|

              AccessNumber { boundingBox :: VBox,
                             sbYear :: SpinButton,
                             sbFirst :: SpinButton,
                             sbLast :: SpinButton,
                             sbNumberOfLabels :: SpinButton,
                             cbLeadingZeros :: CheckButton}

data BackcoverEntry = BackcoverEntry { boxBackcoverEntry :: VBox,
                                       tfMainGroup :: Entry,
                                       tfSubGroup :: Entry,
                                       tfOrganisationTag :: Entry}

data GUIType = MainMenuGUI | LabelCreationGUI SubGUIType | CalibrationGUI
data SubGUIType = BackcoverGUI | AccessNumberGUI
  
main = startGUI

startGUI = do
  -- init GTK
  initGUI

  -- get a GUI Builder
  builder <- getGUIBuilder

  -- create main menu
  mainMenu <- buildGUI MainMenuGUI builder

  -- display main menu
  widgetShowAll $ wMainWindow mainMenu

  -- main loop
  mainGUI

getGUIBuilder :: IO Builder
getGUIBuilder = do
  builder <- builderNew
  file <- getDataFileName "GUI/LabelGenerator.xml"
  builderAddFromFile builder file
  return builder

buildGUI :: GUIType -> Builder -> IO GUI
buildGUI guiType builder = do
  gui <- case guiType of
    MainMenuGUI -> buildMainMenuGUI builder
    LabelCreationGUI BackcoverGUI -> buildLabelCreationGUI BackcoverGUI builder
    LabelCreationGUI AccessNumberGUI -> buildLabelCreationGUI AccessNumberGUI builder
    CalibrationGUI -> buildCalibrationGUI builder

  connectGUI gui

  return gui

connectGUI :: GUI -> IO ()
connectGUI gui@MainMenu{} = connectMainMenuGUI gui
connectGUI gui@Calibration{} = connectCalibrationGUI gui
connectGUI gui@LabelCreation{subGUI=Backcover{}} = connectBackcoverGUI gui
connectGUI gui@LabelCreation{subGUI=AccessNumber{}} = connectAccessNumberGUI gui

changeGUI :: GUI -> GUIType -> IO ()
changeGUI old newType = do

  -- get a fresh GUI Builder
  builder <- getGUIBuilder
  
  -- build new GUI 
  new <- buildGUI newType builder

  -- switch out GUI
  switchOutGUI old new True

switchOutGUI :: GUI -> GUI -> Bool -> IO ()
switchOutGUI old new destroy = do 
  when (destroy == True) $ widgetDestroy (wMainWindow old)
    
  -- display new GUI
  widgetShowAll $ wMainWindow new


-- main menu functions

buildMainMenuGUI :: Builder -> IO GUI
buildMainMenuGUI builder = do
  window <- builderGetObject builder castToWindow "wMainMenu"
  backcover <- builderGetObject builder castToButton "bBackcover"
  accessNumber <- builderGetObject builder castToButton "bAccessNumber"
  calibration <- builderGetObject builder castToButton "bCalibration"
  quit <- builderGetObject builder castToButton "bQuit"

  return $ MainMenu window backcover accessNumber calibration quit

connectMainMenuGUI :: GUI -> IO ()
connectMainMenuGUI gui@(MainMenu w backcover accessnumber calib quit) = do
  w `on` destroyEvent $ liftIO mainQuit >> return True
  w `on` deleteEvent $ liftIO (widgetDestroy w) >> liftIO mainQuit >> return True

  backcover `on` buttonActivated $ changeGUI gui (LabelCreationGUI BackcoverGUI)
  accessnumber `on` buttonActivated $ changeGUI gui (LabelCreationGUI AccessNumberGUI)
  calib `on` buttonActivated $ changeGUI gui CalibrationGUI

  quit `on` buttonActivated $ widgetDestroy w >> mainQuit

  return ()

-- label creation functions

buildLabelCreationGUI :: SubGUIType -> Builder -> IO GUI
buildLabelCreationGUI subType builder = do
  window <- builderGetObject builder castToWindow "wCreateLabels"
  sub <- buildSubGUI  subType builder
  offset <- builderGetObject builder castToCheckButton "cbOffset"
  row <- builderGetObject builder castToSpinButton "sbRow"
  column <- builderGetObject builder castToSpinButton "sbColumn"
  cancel <- builderGetObject builder castToButton "bCancel"
  ok <- builderGetObject builder castToButton "bOK"

  window `set` [windowTitle := "Labelerstellung"]

  -- reparent bounding box of sub GUI
  contentSpace <- builderGetObject builder castToAlignment "alContentSpace"
  widgetReparent (boundingBox sub) contentSpace

  return LabelCreation { wMainWindow = window,
                         subGUI = sub,
                         cbOffset = offset,
                         offsetRows = row,
                         offsetColumns = column,
                         bCancel = cancel,
                         bOK = ok }


buildSubGUI :: SubGUIType -> Builder -> IO SubGUI
buildSubGUI BackcoverGUI = buildBackcoverSubGUI
buildSubGUI AccessNumberGUI = buildAccessNumberSubGUI

-- backcover creation functions

buildBackcoverSubGUI :: Builder -> IO SubGUI
buildBackcoverSubGUI builder = do
  box <- builderGetObject builder castToVBox "boxBackcover"
  add <- builderGetObject builder castToButton "bAddLabel"
  entrySpace <- builderGetObject builder castToVBox "boxEntrySpace"
  
  sub <- addLabelEntry Backcover { boundingBox = box,
                                   boxEntrySpace = entrySpace,
                                   entries = [],
                                   bAddLabel = add}
  return sub

connectBackcoverGUI :: GUI -> IO ()
connectBackcoverGUI gui = do
  (wMainWindow gui) `on` destroyEvent $ liftIO mainQuit >> return True
  (wMainWindow gui) `on` deleteEvent $
    liftIO (widgetDestroy (wMainWindow gui)) >> liftIO mainQuit >> return True
  
  (bCancel gui) `on` buttonActivated $ changeGUI gui MainMenuGUI

  okID <- (bOK gui) `on` buttonActivated $ createBackcoverPdf gui >> changeGUI gui MainMenuGUI
  addID <- (bAdd gui) `on` buttonActivated $ do sub' <- addLabelEntry (subGUI gui)
                                                let gui' = gui{subGUI = sub'} 
                                                switchOutGUI gui gui' False
                                                connectBackcoverGUI gui'
  -- sehr unschöne Lösung
  (bAdd gui) `on` buttonActivated $ do
    signalDisconnect okID
    signalDisconnect addID
    
  return ()
    where bAdd = bAddLabel.subGUI

createBackcoverPdf :: GUI -> IO ()
createBackcoverPdf gui = do
  labels <- mapM getEntryStrings (entries $ subGUI gui)

  (rows,columns) <- getOffset gui backcoverConf
    
  let labelData = StringLabel labels
  printSet <- loadCalibration
  let conf = offsetLabelPosition backcoverConf (rows-1) (columns-1)

  filename <- createLabelPdf (Settings conf printSet) $ parseLabelData labelData
  showPdf filename

getEntryStrings :: BackcoverEntry -> IO (String,String,String)
getEntryStrings entry = do
  mainGroup <- entryGetText (tfMainGroup entry)
  subGroup <- entryGetText (tfSubGroup entry)
  tag <- entryGetText (tfOrganisationTag entry)

  return (mainGroup, subGroup, tag)
  
addLabelEntry :: SubGUI -> IO SubGUI
addLabelEntry old@Backcover{} = do
  newEntry <- backcoverEntry

  widgetReparent (boxBackcoverEntry newEntry) (boxEntrySpace old)

  return old{entries=(entries old)++[newEntry]}
addLabelEntry _ = error "can't add LabelEntry to this GUI type"

backcoverEntry :: IO BackcoverEntry
backcoverEntry = do
  builder <- builderNew
  file <- getDataFileName "./GUI/BackcoverEntry.xml"
  builderAddFromFile builder file

  box <- builderGetObject builder castToVBox "backcoverEntry"
  mainGroup <- builderGetObject builder castToEntry "tfMainGroup"
  subGroup <- builderGetObject builder castToEntry "tfSubGroup"
  tag <- builderGetObject builder castToEntry "tfOrganisationTag"
  
  return $ BackcoverEntry box mainGroup subGroup tag

-- accessnummber creation functions

buildAccessNumberSubGUI :: Builder -> IO SubGUI
buildAccessNumberSubGUI builder = do
  box <- builderGetObject builder castToVBox "boxAccessNumber"
  year <- builderGetObject builder castToSpinButton "sbYear"
  first <- builderGetObject builder castToSpinButton "sbFirst"
  last <- builderGetObject builder castToSpinButton "sbLast"
  number <- builderGetObject builder castToSpinButton "sbNumberOfLabels"
  zeros <- builderGetObject builder castToCheckButton "cbLeadingZeros"
    
  return AccessNumber { boundingBox = box,
                        sbYear = year,
                        sbFirst = first,
                        sbLast = last,
                        sbNumberOfLabels = number,
                        cbLeadingZeros = zeros}

connectAccessNumberGUI :: GUI -> IO ()
connectAccessNumberGUI gui = do
  (wMainWindow gui) `on` destroyEvent $ liftIO mainQuit >> return True
  (wMainWindow gui) `on` deleteEvent $
    liftIO (widgetDestroy (wMainWindow gui)) >> liftIO mainQuit >> return True

  (bCancel gui) `on` buttonActivated $ changeGUI gui MainMenuGUI
  -- TODO: Preview
  (bOK gui) `on` buttonActivated $ createAccessNumberPdf gui >> changeGUI gui MainMenuGUI
  return ()

createAccessNumberPdf :: GUI -> IO ()
createAccessNumberPdf gui = do
  year <- spinButtonGetValueAsInt $ sbYear $ subGUI gui
  first <- spinButtonGetValueAsInt $ sbFirst $ subGUI gui
  last <- spinButtonGetValueAsInt $ sbLast $ subGUI gui

  number <- spinButtonGetValueAsInt $ sbNumberOfLabels $ subGUI gui
  zeros <- toggleButtonGetActive $ cbLeadingZeros $ subGUI gui

  (rows,columns) <- getOffset gui indexConf
           
  let labelData = RangeLabel year first last zeros
  printSet <- loadCalibration
  let conf = offsetLabelPosition indexConf{numberOfLabels=number} (rows-1) (columns-1)

  filename <- createLabelPdf (Settings conf printSet) $ parseLabelData labelData 
  showPdf filename
  
-- calibration functions

buildCalibrationGUI :: Builder -> IO GUI
buildCalibrationGUI builder = do
  window <- builderGetObject builder castToWindow "wCalibration"
  save <- builderGetObject builder castToButton "bCalibSave"
  cancel <- builderGetObject builder castToButton "bCalibCancel"

  top <- builderGetObject builder castToSpinButton "sbTopOffset"
  bottom <- builderGetObject builder castToSpinButton "sbBottomOffset"
  left <- builderGetObject builder castToSpinButton "sbLeftOffset"
  right <- builderGetObject builder castToSpinButton "sbRightOffset"

  print <- builderGetObject builder castToButton "bPrintTestPage"

  -- get and display printer settings
  settings <- loadCalibration

  top `set` [spinButtonValue := (topOffset settings) ]
  bottom `set` [spinButtonValue := (bottomOffset settings) ]
  left `set` [spinButtonValue := (leftOffset settings) ]
  right `set` [spinButtonValue := (rightOffset settings) ]

  return Calibration { wMainWindow = window,
                       sbTopOffset = top,
                       sbBottomOffset = bottom,
                       sbLeftOffset = left,
                       sbRightOffset = right,
                       bPrintTestPage = print,
                       bCalibCancel = cancel,
                       bCalibSave = save} 

connectCalibrationGUI :: GUI -> IO ()
connectCalibrationGUI gui = do
  (wMainWindow gui) `on` destroyEvent $ liftIO mainQuit >> return True
  (wMainWindow gui) `on` deleteEvent $
    liftIO (widgetDestroy (wMainWindow gui)) >> liftIO mainQuit >> return True
  
  (bCalibCancel gui) `on` buttonActivated $ changeGUI gui MainMenuGUI

  (bPrintTestPage gui) `on` buttonActivated $ do
    filename <- createCalibrationPage
    showPdf filename
    changeGUI gui MainMenuGUI

  (bCalibSave gui) `on` buttonActivated $ do
    cal <- createCalibration gui
    saveCalibration cal
    changeGUI gui MainMenuGUI
  
  return ()

createCalibration :: GUI -> IO PrinterSetting
createCalibration gui = do
  top <- (sbTopOffset gui) `get` spinButtonValue
  bottom <- (sbBottomOffset gui) `get` spinButtonValue
  left <- (sbLeftOffset gui) `get` spinButtonValue
  right <- (sbRightOffset gui) `get` spinButtonValue

  return PrinterSetting {topOffset = top,
                         bottomOffset = bottom,
                         leftOffset = left,
                         rightOffset = right}

-- helper functions

getOffset :: GUI -> Configuration -> IO (Int,Int)
getOffset gui conf = do
  useOffset <- toggleButtonGetActive $ cbOffset gui
  rows <- spinButtonGetValueAsInt $ offsetRows gui
  columns <- spinButtonGetValueAsInt $ offsetColumns gui

  let offset = if useOffset
               then (rows,columns)
               else (0,0)

  return offset
  
showPdf :: FilePath ->  IO ()
showPdf filename = do
  forkIO $ rawSystem "xdg-open" [filename] >> return() -- >> removeFile filename
  return ()
  
