{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Control.Applicative 
import Data.List (transpose)

align :: [Text] -> Text -> [Text]
align lines alignChar =
    let lines' = map (T.splitOn alignChar) lines
        firstCol:rest = transpose lines'
        firstCol' = map (`T.append` alignChar) $ adjustWidth firstCol
        lines'' = transpose (firstCol':(fixLeft rest))
    in map T.concat lines''

-- | Makes column cells in a column the same width
adjustWidth :: [Text] -> [Text]
adjustWidth xs = 
    let maxWidth = maximum $ map T.length xs
    in map (T.justifyLeft maxWidth ' ') xs

-- | strips whitespace around text but makes sure that it's left-padded with one space
trim :: Text -> Text
trim s =  " " `T.append` T.strip s

-- | Fixes the spacing on the left side of columns
fixLeft :: [[Text]] -> [[Text]]
fixLeft [] = []
fixLeft (x:xs) = (map trim x):xs

main = do
  alignStrings <- (concat . map (T.words . T.pack)) <$> getArgs 
  input <- (T.lines . T.pack) <$> getContents
  let result = foldl (\lines sep -> align lines sep) input alignStrings 
  T.putStr . T.unlines $ result
