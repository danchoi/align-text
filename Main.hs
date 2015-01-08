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
  , matchStrings :: [Text]
  } deriving (Show)

data MatchMode = Series | Alternatives  deriving Show

parseOpts :: Parser Options
parseOpts = Options 
    <$> flag Series Alternatives
          (short 'a' <> long "alternatives" 
          <> help "Treat match strings as alternatives for alignment, like regex /(a|b|c)/.")
    <*> ((T.words . T.pack) <$> 
          (argument str
             (metavar "MATCH STRINGS" 
             <> help "The strings to align on, between a pair of single quotes")))

opts = info (helper <*> parseOpts)
            (fullDesc <> progDesc "Align code text from STDIN on operators."
                      <> header "align"
                      <> footer "See https://github.com/danchoi/align for more info.")

main = do
  Options mode alignStrings <- execParser opts
  print alignStrings
  input <- (T.lines . T.pack) <$> getContents
  let result :: [Text]
      result =
        case mode of 
          Series -> foldl (\lines sep -> align lines sep) input alignStrings 
          Alternatives -> alignOnAlteratives alignStrings input 
  T.putStr . T.unlines $ result

-- Aligning in standard Series mode
align :: [Text] -> Text -> [Text]
align lines alignString =
    -- change to T.breakOn :: Text -> Text -> (Text, Text)
    let lines' :: [[Text]]
        lines'        = map (splitOn alignString) lines
        firstCol:rest = transpose lines'
        firstCol'     = adjustWidth alignString firstCol
        lines''       = transpose (firstCol':(fixLeft rest))
    in map T.concat lines''

-- split a line into two segments if it contains the alignstring, 
-- and one if it doesn't
splitOn :: Text -> Text -> [Text]
splitOn alignString input =
    let (x,y) = T.breakOn alignString input
    in [z | z <- [x,y], z /= mempty]
    
-- | Makes column cells in a column the same width
-- Used for the standard Series mode.
adjustWidth :: Text -> [Text] -> [Text]
adjustWidth alignStr xs = 
    map maybeAdjust xs
  where maxWidth = maximum $ map (T.length . T.stripEnd) $ xs
        maybeAdjust :: Text -> Text
        maybeAdjust cell = T.justifyLeft maxWidth ' ' . T.stripEnd $ cell

alignOnAlteratives :: [Text] -> [Text] -> [Text]
alignOnAlteratives alts lines =
    -- each row cell contains (Maybe alternative) that was split on
    let lines' :: [[Text]]  
        lines' = map (splitOnAny alts) lines
        firstCol:rest = transpose lines'
        firstCol' = adjustWidth' firstCol
        lines'' = transpose (firstCol':rest)
    in map T.concat lines''

-- | adjust width of a column of cells
adjustWidth' :: [Text] -> [Text]
adjustWidth' xs =
    map adj xs
  where maxWidth = maximum $ map (T.length . T.stripEnd) xs
        adj cell = T.justifyLeft maxWidth ' ' . T.stripEnd $ cell


-- | Attempts to split line on any of the alternatives
-- and returns the segments
splitOnAny :: [Text] -> Text -> [Text]
splitOnAny alts line = 
    let alts' = filter (`T.isInfixOf` line) alts
    in if null alts'
       then [line]
       else let alt = head alts'
                (x, y) = T.breakOn alt line
            in [x,y] -- y is guaranteed to be not mempty

-- | strips whitespace around text but makes sure that it's left-padded with one space
trim :: Text -> Text
trim s =  " " `T.append` T.strip s

-- | Fixes the spacing on the left side of the first column of the rest of the line
fixLeft :: [[Text]] -> [[Text]]
fixLeft [] = []
fixLeft (x:xs) = (map trim x):xs
