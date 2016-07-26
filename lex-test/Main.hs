{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Applicative
import Parser (chunk, chunkOrElse, singleQuotes, unicodeSyntax)
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
                (x:_) -> printf "  %s %s" (uord x) x
  where uord [] = "U+----"
        uord (c:_) = printf "U+%04x" (ord c)

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

data ParseMode = Default | SingleQuotes | UnicodeSyntax
  deriving (Show)

analyzeSource :: ParseMode -> FilePath -> IO Result
analyzeSource parseMode path = do
  contents <- BS.readFile path
  let text = T.decodeUtf8' contents
      tokenize = case parseMode of
                   Default       -> chunk
                   SingleQuotes  -> chunkOrElse singleQuotes
                   UnicodeSyntax -> chunkOrElse (\s -> (singleQuotes s <|> unicodeSyntax s))
  case text of
    Left _  -> return DecodeError
    Right t -> case dropWhile isRight (tokenize (T.unpack t)) of
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
  args <- getArgs
  let parseMode = case args of
                    [] -> Default
                    ("singleQuotes": _)  -> SingleQuotes
                    ("unicodeSyntax": _) -> UnicodeSyntax
                    (arg: _)             -> error $ "unknown tokenizer mode: " ++ arg
  contents <- getContents
  let paths = words contents
  start <- getCurrentTime
  let npaths = length paths

  let loop stats [] = return stats
      loop stats (p:ps) = do
        result <- analyzeSource parseMode p
        stats' <- do case result of
                       DecodeError -> do putStrLn $ "DECODE ERROR: " ++ p
                                         return $ over decode_errors (+1) stats
                       Ok          -> do putStrLn $ "OK: " ++ p
                                         return $ over ok_count (+1) stats
                       NotOk []    -> do putStrLn $ "OK: " ++ p
                                         return $ over ok_count (+1) stats
                       NotOk (c:cs) -> do putStrLn $ "NOT OK: " ++ p
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
  -- dump the stats record
  putStrLn "Stats:"
  putStrLn $ printf "  count        : %5d" (_count stats)
  putStrLn $ printf "  decode errors: %5d" (_decode_errors stats)
  putStrLn $ printf "  ok           : %5d" (_ok_count stats)
  putStrLn $ printf "  not ok       : %5d" (_error_count stats)
  putStrLn ""
  -- dump the first_char map
  putStrLn "First Character Errors:"
  forM_ (Map.assocs (view first_char stats)) $ \(ch,cnt) -> do
    putStrLn $ printf "  U+%04x - %6d - %c" (ord ch) cnt ch

