{-# LANGUAGE BangPatterns, TemplateHaskell #-}

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
import qualified Data.Map.Strict as Map
import Control.Lens
import Data.Char

isRight (Right _) = True
isRight _         = False

fmtNotOk str = case take 2 (lines str) of
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

data Stats = Stats { _count         :: !Int
                   , _decode_errors :: !Int
                   , _ok_count      :: !Int
                   , _error_count   :: !Int
                   , _first_char    :: Map.Map Char Int
                   }
  deriving (Show)

makeLenses ''Stats

zeroStats = Stats 0 0 0 0 Map.empty

data Result = DecodeError | Ok | NotOk String
  deriving (Show, Read)

analyzeSource :: FilePath -> IO Result
analyzeSource path = do
  contents <- BS.readFile path
  let text = T.decodeUtf8' contents
  case text of
    Left _  -> return DecodeError
    Right t -> case dropWhile isRight (chunk (T.unpack t)) of
                 [] -> return Ok
                 (Left str:_)  -> return (NotOk str)

addChar :: Char -> Map.Map Char Int -> Map.Map Char Int
addChar ch m  = Map.insertWith (+) ch 1 m

fmtProgress :: UTCTime -> Stats -> UTCTime -> Int -> String
fmtProgress start stats now nfiles =
  let delta = fromEnum $ diffUTCTime now start
      dsecs = div delta (10^12)
      (mins,secs) = divMod dsecs 60
      percent = div (ncount * 100) nfiles
      ncount = view count stats
      ndecode = view decode_errors stats
      nok = view ok_count stats
      nnotok = view error_count stats
      bar = printf "Files Processed: %6d / %6d  %3d %%  DECODE: %d  OK: %d  NOT OK: %d  TIME: %2d:%02d" ncount nfiles percent ndecode nok nnotok mins secs
  in bar

showProgress :: UTCTime -> Int -> Stats -> IO ()
showProgress start nfiles stats = do
  now <- getCurrentTime
  let bar = fmtProgress start stats now nfiles
  hPutStr stderr $ "\r" ++ bar
  hFlush stderr

main = do
  contents <- getContents
  let paths = words contents
  start <- getCurrentTime
  let npaths = length paths

  let loop stats [] = return stats
      loop stats (p:ps) = do
        result <- analyzeSource p
        stats' <- do case result of
                       DecodeError -> do putStrLn $ p ++ ": DECODE ERROR"
                                         return $ over decode_errors (+1) stats
                       Ok          -> do putStrLn $ p ++ ": OK"
                                         return $ over ok_count (+1) stats
                       NotOk []    -> do putStrLn $ p ++ ": OK"
                                         return $ over ok_count (+1) stats
                       NotOk (c:cs) -> do putStrLn $ p ++ ": NOT OK"
                                          putStrLn $ fmtNotOk (c:cs)
                                          let m = addChar c (stats ^. first_char)
                                          return $ set first_char m $ over error_count (+1) $ stats
        let stats'' = over count (+1) stats'

        when (mod (view count stats'') 10 == 0) $ do
          showProgress start npaths stats''
        loop stats'' ps

  stats <- loop zeroStats paths
  showProgress start npaths stats
  hPutStrLn stderr "\n"
  -- dump the first_char map
  forM_ (Map.assocs (view first_char stats)) $ \(ch,cnt) -> do
    putStrLn $ printf "  U+%04x - %6d - %c" (ord ch) cnt ch

