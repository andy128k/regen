module Main (main) where

import Regen
import Options.Applicative


type InputFile = Maybe FilePath

readInputFile :: String -> Either String InputFile
readInputFile ""  = Left "Bad filename"
readInputFile "-" = Right Nothing
readInputFile s   = Right (Just s)


data OutputFormat = Dump | Pretty | Lisp

readOutputFormat :: String -> Either String OutputFormat
readOutputFormat "dump"   = Right Dump
readOutputFormat "pretty" = Right Pretty
readOutputFormat "lisp"   = Right Lisp
readOutputFormat _        = Left "Bad printer"

instance Show OutputFormat where
    show Dump = "dump"
    show Pretty = "pretty"
    show Lisp = "lisp"


data Opts = Opts
  { input :: InputFile
  , format :: OutputFormat }


options = info (helper <*> flags)
          ( fullDesc
         <> progDesc "Generate regular expression matching given list of strings" )
    where
      flags :: Parser Opts
      flags = Opts
        <$> option (eitherReader readInputFile)
                ( long "file"
               <> short 'f'
               <> metavar "FILE"
               <> value Nothing
               <> showDefault
               <> help "Input file" )
        <*> option (eitherReader readOutputFormat)
                ( long "format"
               <> short 'p'
               <> metavar "FORMAT"
               <> value Pretty
               <> showDefault
               <> help "Output format (dump, pretty, lisp)" )


readInput :: InputFile -> IO String
readInput (Just filename) = readFile filename
readInput Nothing         = getContents


greet :: Opts -> IO ()
greet (Opts input format) = do
  file <- readInput input
  print $ squeeze $ Alt $ range $ lines file


main :: IO ()
main = execParser options >>= greet


-- #+nil
-- (loop
--    for (from to) in '(("0" "9")
-- 		      ("0" "0")
-- 		      ("000" "999")
-- 		      ("0999" "1100")
-- 		      ("000" "009")
-- 		      ("3" "7")
-- 		      ("01" "10")
-- 		      ("01" "30")
-- 		      ("001" "365")
-- 		      )
--    do (format t "(~A ~A) -> ~A~%"
-- 	      from to
-- 	      (range1 (coerce from 'list)
-- 		      (coerce to 'list))))

  --   print $ squeeze (range [1, 2..365] 7)
  --   print $ squeeze (range [3, 4..7] 3)
  --   print $ squeeze (range [3, 5..37] 0)

