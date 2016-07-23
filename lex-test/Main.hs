
module Main where

import Control.Monad
import Parser (chunk)
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.IO as T


isRight (Right _) = True
isRight _         = False

fmt str = case take 2 (lines str) of
            [] -> "  (empty)"
            [x] -> "  " ++ x
            (x:_) -> unlines ["  " ++ x, "  ..."]

main = do
  contents <- getContents
  let paths = words contents
  forM_ paths $ \path -> do
    contents <- BS.readFile path
    let text = T.decodeUtf8' contents
    case text of
      Left _  -> putStrLn $ path ++ ": DECODE ERROR"
      Right t -> case dropWhile isRight (chunk (T.unpack t)) of
                   [] -> putStrLn $ path ++ ": OK"
                   (Left str:_)  -> do putStrLn $ path ++ ": NOT OK"
                                       putStrLn (fmt str)

