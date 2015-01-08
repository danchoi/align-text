{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Control.Applicative 
import Data.List (transpose)
import Data.Monoid
import Options.Applicative

data Options = Options {
    matchMode :: MatchMode 
  } deriving (Show)

data MatchMode = Series | Alternatives  deriving Show

parseOpts :: Parser Options
parseOpts = Options 
    <$> flag Series Alternatives
          (short 'a' <> long "--alternatives" 
          <> help "Treat match strings as alternatives for alignment, like regex /(a|b|c)/.")

opts = info (helper <*> parseOpts)
            (fullDesc <> progDesc "Align code text from STDIN on operators."
                      <> header "align"
                      <> footer "See https://github.com/danchoi/align for more info.")

align :: [Text] -> Text -> [Text]
align lines alignString =
    let lines' = map (T.splitOn alignString) lines
        firstCol:rest = transpose lines'
        firstCol' = adjustWidth alignString firstCol
        lines'' = transpose (firstCol':(fixLeft alignString rest))
    in map T.concat lines''

-- | Makes column cells in a column the same width
adjustWidth :: Text -> [Text] -> [Text]
adjustWidth alignStr xs = 
    -- TODO, skip lines that are just interleaved comments $ filter (alignStr `T.isInfixOf`)
    let maxWidth = maximum $ map (T.length . T.stripEnd)  xs
    in map (T.justifyLeft maxWidth ' ' . T.stripEnd) xs

-- | strips whitespace around text but makes sure that it's left-padded with one space
trim :: Text -> Text
trim s =  " " `T.append` T.strip s

-- | Fixes the spacing on the left side of the first column of the rest of the line
fixLeft :: Text -> [[Text]] -> [[Text]]
fixLeft alignString [] = []
fixLeft alignString (x:xs) = 
    let x' = map (prepend alignString) x
    in (map trim x'):xs
  where prepend :: Text -> Text -> Text
        prepend _ x | x == mempty = mempty
        prepend a x = mconcat [a, trim x]

main = do
  Options mode <- execParser opts
  alignStrings <- (concat . map (T.words . T.pack)) <$> getArgs 
  input <- (T.lines . T.pack) <$> getContents
  let result = foldl (\lines sep -> align lines sep) input alignStrings 
  T.putStr . T.unlines $ result
