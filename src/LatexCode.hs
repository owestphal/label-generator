module LatexCode
       (Settings(..)
       ,Configuration(..)
       ,PrinterSetting(..)
       ,defaultPrinterSettings
       ,generateLatexFileContent
       ,indexConf
       ,backcoverConf
       ,calibConf
       )where

data Settings = Settings Configuration PrinterSetting

-- all distances are milimeter values
data Configuration = Configuration { columns        :: Int,
                                     rows           :: Int,
                                     numberOfLabels :: Int,
                                     leftMargin     :: Double,
                                     rightMargin    :: Double,
                                     topMargin      :: Double,
                                     bottomMargin   :: Double,
                                     labelDistH     :: Double,
                                     labelDistV     :: Double,
                                     hasBorders     :: Bool,
                                     offset         :: Int}

-- positive value if printer prints to close to edge [in mm]
-- negative value if printer prints to far from edge [in mm]
data PrinterSetting = PrinterSetting { topOffset    :: Double,
                                       bottomOffset :: Double,
                                       rightOffset  :: Double,
                                       leftOffset   :: Double }
                      deriving (Show, Read)

defaultPrinterSettings :: PrinterSetting
defaultPrinterSettings = PrinterSetting 0 0 0 0

generateLatexFileContent :: Settings -> [String] -> String
generateLatexFileContent set@(Settings conf _) labels =
  preamble set ++ "\n" ++ body conf labels

preamble :: Settings -> String
preamble set =
    " \\documentclass[12pt,a4paper]{article} \n \
    \ \\usepackage[utf8]{inputenc} \n \
    \ \\usepackage[german]{babel} \n \
    \ \\usepackage[T1]{fontenc} \n \
    \ \\usepackage{helvet} \n \
    \ \\renewcommand{\\familydefault}{\\sfdefault} \n \
    \ \\usepackage[newdimens]{labels} \n \
    \ \\usepackage{graphicx} \n \
    \ \\usepackage{calc} \n"
    ++ settings set ++
    " \\newcommand{\\mkLabel}[1]{  \
    \ \\genericlabel{ \
    \ \\begin{tabular}{l} #1 \\end{tabular} \
    \ }}"

settings :: Settings -> String
settings (Settings conf printer) =
    " \\LabelCols=" ++ show (columns conf) ++ " \
    \ \\LabelRows=" ++ show (rows conf) ++ " \

    \ \\LeftPageMargin=" ++ show (offL + leftMargin conf) ++ "mm \
    \ \\RightPageMargin=" ++ show (offR + rightMargin conf) ++ "mm \
    \ \\TopPageMargin=" ++ show (offT + topMargin conf) ++ "mm \
    \ \\BottomPageMargin=" ++ show (offB + bottomMargin conf) ++ "mm \
    \ \\InterLabelColumn=" ++ show (labelDistH conf) ++ "mm \
    \ \\InterLabelRow=" ++ show (labelDistV conf) ++ "mm \
    \ \\LeftLabelBorder=0mm \
    \ \\RightLabelBorder=0mm \
    \ \\TopLabelBorder=0mm \
    \ \\BottomLabelBorder=0mm " ++
    if hasBorders conf
      then "\\LabelGridtrue"
      else ""
      where offT = topOffset printer
            offB = bottomOffset printer
            offR = rightOffset printer
            offL = leftOffset printer


body :: Configuration -> [String] -> String
body conf labels = "\\begin{document}"
              ++ concat (replicate (offset conf) $ makeLabel "")
              ++ concatMap
                 (concat .replicate (numberOfLabels conf).makeLabel)
                 labels
              ++ "\\end{document}"

makeLabel :: String -> String
makeLabel text = "\\mkLabel{" ++ text ++ "}"

--TODO namen überdenken
backcoverConf :: Configuration
backcoverConf = Configuration { columns = 9,
                                rows = 7,
                                numberOfLabels = 1,
                                leftMargin = 11,
                                rightMargin = 11,
                                topMargin = 9,
                                bottomMargin = 9,
                                labelDistH = 2,
                                labelDistV = 2,
                                hasBorders = False,
                                offset = 0}

indexConf :: Configuration
indexConf = Configuration { columns = 7,
                            rows = 27,
                            numberOfLabels = 2,
                            leftMargin = 10,
                            rightMargin = 8,
                            topMargin = 13.5,
                            bottomMargin = 13.5,
                            labelDistH = 3,
                            labelDistV = 0,
                            hasBorders = False,
                            offset = 0}

calibConf :: Configuration
calibConf =  Configuration { columns = 1,
                            rows = 1,
                            numberOfLabels = 1,
                            leftMargin = 10,
                            rightMargin = 10,
                            topMargin = 10,
                            bottomMargin = 10,
                            labelDistH = 0,
                            labelDistV = 0,
                            hasBorders = True,
                            offset = 0}
