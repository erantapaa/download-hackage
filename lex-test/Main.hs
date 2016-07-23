
module Main where

import Control.Monad
import Parser (chunk)
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Data.Time.Clock
import Text.Printf


isRight (Right _) = True
isRight _         = False

fmt str = case take 2 (lines str) of
            [] -> "  (empty)"
            [x] -> "  " ++ x
            (x:_) -> unlines ["  " ++ x, "  ..."]

enumerate xs = zip [(1::Int)..] xs

fmtElapsedTime start end =
  let delta = fromEnum $ diffUTCTime end start
      dsecs = div delta (10^12)
      (mins,secs) = divMod dsecs 60
  in printf "%d:%02d" mins secs :: String

fmtProgressBar start end count nfiles =
  let delta = fromEnum $ diffUTCTime end start
      dsecs = div delta (10^12)
      (mins,secs) = divMod dsecs 60
      percent = div (count*100) nfiles
  in printf "%6d / %6d  %3d %%  elapsed: %2d:%02d" count nfiles percent mins secs

main = do
  contents <- getContents
  let paths = words contents
  start <- getCurrentTime
  let npaths = length paths
  forM_ (enumerate paths) $ \(i,path) -> do

    contents <- BS.readFile path
    let text = T.decodeUtf8' contents
    case text of
      Left _  -> putStrLn $ path ++ ": DECODE ERROR"
      Right t -> case dropWhile isRight (chunk (T.unpack t)) of
                   [] -> putStrLn $ path ++ ": OK"
                   (Left str:_)  -> do putStrLn $ path ++ ": NOT OK"
                                       putStrLn (fmt str)
    when (mod i 100 == 0) $ do
      now <- getCurrentTime
      hPutStr stderr $ "\rFiles processed: " ++ fmtProgressBar start now i npaths
      hFlush stderr
  hPutStr stderr "\n"

