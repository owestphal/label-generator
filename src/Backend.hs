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

main = undefined

createLabelPdf :: Settings -> [String] -> IO FilePath
createLabelPdf set ls = do
  (filepath, handle) <- openTempFile "" "result.tex"
  hPutStr handle $ generateLatexFileContent set ls
  hClose handle
  rawSystem "pdflatex" [ "-output-directory", ".", filepath]

  let filepathBase = fst.breakAtDot $ filepath
  
  removeFiles filepathBase [".aux",".log",".tex"]
  return $ filepathBase ++ ".pdf"
  
removeFiles :: String -> [String] -> IO ()
removeFiles basename suffixes  = mapM_ removeFile $ zipWith (++) (repeat.fst.breakAtDot $ basename) suffixes

breakAtDot = break ( (==) '.' ) 

-- Label format functions

parseLabelData :: LabelData -> [String]
parseLabelData (RangeLabel year f l True) = map ( ((show year ++ "/") ++).addZeros.show ) [f..l]
parseLabelData (RangeLabel year f l False) = map ( ((show year ++ "/") ++).show ) [f..l]
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
    otherwise -> s 

testIndexLabels = createLabelPdf (Settings indexConf printer)
                  $ parseLabelData (RangeLabel 2015 1 500 True) 

-- backcover Label functions

testBackcoverLabels = createLabelPdf (Settings backcoverConf printer) $ parseLabelData testData

testData = StringLabel $ [("Ge","5.42", "Thies"), ("Sp","2.29","Dawid")] ++ blanks

blanks = take 51 $ repeat ("","","")

-- calibration functions

createCalibrationPage = createLabelPdf (Settings calibConf printer)
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
                  
printer = defaultPrinterSettings  {topOffset = 1}
