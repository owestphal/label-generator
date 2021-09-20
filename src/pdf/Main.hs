import System.Environment
import Backend
import LatexCode

import Control.Monad (void)
import System.Directory (copyFile)

main :: IO ()
main = void $ do
  args <- getArgs
  case args of
    -- calibration page
    ["--calibration-page",target] -> do
      path <- createCalibrationPage
      copyFile path target
    -- set calibration
    ["--print-settings", top, bottom, left, right] -> do
      saveCalibration $ PrinterSetting (read top) (read bottom) (read left) (read right)
    -- create labels
    [year,f,l,n,target] -> do
      printerSettings <- loadCalibration
      let
        number = read n
        conf = offsetLabelPosition indexConf{numberOfLabels=number} 0 0
      path <- createLabelPdf (Settings conf printerSettings) $ parseLabelData (RangeLabel (read year) (read f) (read l) True)
      copyFile path target
    _ -> putStrLn "unknown parameters"
