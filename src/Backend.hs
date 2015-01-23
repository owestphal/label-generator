module Backend
       (LabelData(..)
       ,createLabelPdf
       ,createCalibrationPage
       ,parseLabelData
       ,offsetLabelPosition
       ,saveCalibration
       ,loadCalibration
       ) where

import System.IO
import System.Directory
import System.Process
import System.Posix.User

import Control.Exception.Base

import LatexCode

data LabelData = RangeLabel {year :: Int, first :: Int, last :: Int, zeros :: Bool } |
                 StringLabel {labels :: [(String,String,String)]}

createLabelPdf :: Settings -> [String] -> IO FilePath
createLabelPdf set ls = do
  (filepath, fileH) <- openTempFile "/tmp" "result.tex"
  hPutStr fileH $ generateLatexFileContent set ls
  hClose fileH
  -- TODO: maybe catch latex-compile error
  _ <- rawSystem "pdflatex" [ "-output-directory", "/tmp", filepath]

  let filepathBase = fst.breakAtDot $ filepath
  
  removeFiles filepathBase [".aux",".log",".tex"]
  return $ filepathBase ++ ".pdf"
  
removeFiles :: String -> [String] -> IO ()
removeFiles basename suffixes  = mapM_ removeFile $ zipWith (++) (repeat.fst.breakAtDot $ basename) suffixes

breakAtDot :: String -> (String,String)
breakAtDot = break ( (==) '.' ) 

-- Label format functions

parseLabelData :: LabelData -> [String]
parseLabelData (RangeLabel y f l True) = map ( ((show y ++ "/") ++).addZeros.show ) [f..l]
parseLabelData (RangeLabel y f l False) = map ( ((show y ++ "/") ++).show ) [f..l]
parseLabelData (StringLabel l) = map (\(s1,s2,s3) -> s1++ nl ++s2++ nl4 ++(take 5 s3)) l
  where nl = "\\\\ " -- double backslash ( = newline in LaTex)
        nl4 = concat [nl,nl,nl,nl]
        
computeOffset :: Configuration -> Int -> Int -> Int
computeOffset Configuration {columns = cs} r c = c + r * cs

offsetLabelPosition :: Configuration -> Int -> Int -> Configuration
offsetLabelPosition conf r c = conf{offset=(computeOffset conf r c)}

-- index Label functions

addZeros :: String -> String
addZeros s = case length s of
    0 -> "000"
    1 -> "00" ++ s
    2 -> "0" ++ s
    3 -> "" ++ s
    _ -> s 

-- calibration functions

createCalibrationPage :: IO FilePath
createCalibrationPage = do
  printer <- loadCalibration
  createLabelPdf (Settings calibConf printer)
    ["Drucker Test Seite: \\\\ Alle Kanten sollten 10 mm vom Rand entfernt sein.\\\\ Falls nicht muss das Programm nach kalibriert werden.\\\\ Ein positiver Kalibrierungswert verschiebt die Kante in Richtung Blattmitte,\\\\ ein negativer Wert in Richtung Rand."]

saveCalibration :: PrinterSetting -> IO ()
saveCalibration set = do
  user <- getEffectiveUserName
  writeFile ("/home/" ++ user ++ "/.labelGeneratorPrinterSettings") $ show set 

  
loadCalibration :: IO PrinterSetting
loadCalibration = do
  user <- getEffectiveUserName
  (readFile ("/home/" ++ user ++ "/.labelGeneratorPrinterSettings") >>= (\s -> return $ read s) )
    `catch` handleError
  where handleError :: IOException -> IO PrinterSetting
        handleError _ = return defaultPrinterSettings

