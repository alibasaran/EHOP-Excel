{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Bool
import Data.List.Extra
import Data.Maybe
import System.Exit

import qualified Data.Map as Map

import ExcelBuild
import Data.Time

{-
    Inspired from https://github.com/snowleopard/build
    Runtime and memory analysis script used in the research
-}

inputCells :: [Cell]
inputCells = [ "A1", "A2", "A3" ]

inputs :: i -> Store i Cell Int
inputs i = initialise i $ \cell -> fromMaybe 0 $ lookup cell $ zip inputCells [1..]

spreadsheet :: Spreadsheet
----------------
-- Setup 1
spreadsheet "B1" = Just 1                         
spreadsheet "B2" = Just $ "B1" + 1                
spreadsheet "B3" = Just $ "A3" * "B2"             
spreadsheet "C1" = Just $ "B2" - "B3"
spreadsheet "C2" = Just $ "C1" + "B3"  
spreadsheet "C3" = Just 5             
spreadsheet "F0" = Just 2                         
spreadsheet "F1" = Just 1                         
spreadsheet "F2" = Just $ "C3" + ("C1" * "B3")                        
spreadsheet  _   = Just 0
--

targets :: [Cell]
targets = [ "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "F0", "F1", "F2", "F3", "F4" ]

tasks :: Tasks Monad Cell Int
tasks = spreadsheetTask spreadsheet

test :: String -> Build Monad i Cell Int -> i -> IO Bool
test name build i = do
    let store   = inputs i
        result  = sequentialMultiBuild build tasks targets store
        correct = all (correctBuild tasks store result) targets
    putStr $ name ++ " is "
    case (trim name, correct) of
        ("dumb", False) -> do putStr "incorrect, which is [OK]\n"; return True
        (_     , False) -> do putStr "incorrect: [FAIL]\n"       ; return False
        (_     , True ) -> do putStr "correct: [OK]\n"           ; return True

testSuite :: IO Bool
testSuite = and <$> sequence
    [ test  "excel     " excel      (const True, mempty) ]

main :: IO ()
main = do
    success <- testSuite
    unless success $ die "\n========== At least one test failed! ==========\n"

