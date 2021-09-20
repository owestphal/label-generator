import System.Environment
import Backend
import LatexCode

import Control.Monad (void)
import System.Directory (copyFile)

main :: IO ()
main = void $ do
  args <- getArgs
  case args of
    [year,f,l,n,target] -> do
      printerSettings <- loadCalibration
      let
        number = read n
        conf = offsetLabelPosition indexConf{numberOfLabels=number} 0 0
      path <- createLabelPdf (Settings conf printerSettings) $ parseLabelData (RangeLabel (read year) (read f) (read l) True)
      copyFile path target
    ["--calibration-page",target] -> do
      path <- createCalibrationPage
      copyFile path target
    _ -> putStrLn "unknown parameters"
