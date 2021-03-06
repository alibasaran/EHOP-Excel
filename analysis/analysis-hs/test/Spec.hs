{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Bool
import Data.List.Extra
import Data.Maybe
import System.Exit

import qualified Data.Map as Map

import ExcelBuild

{-
    A single file containing __only__ the Excel build testing code 
    from https://github.com/snowleopard/build
-}

inputCells :: [Cell]
inputCells = [ "A1", "A2", "A3" ]

inputs :: i -> Store i Cell Int
inputs i = initialise i $ \cell -> fromMaybe 0 $ lookup cell $ zip inputCells [1..]

spreadsheet :: Spreadsheet
spreadsheet cell = case name cell of
    "B1"  -> Just 1                         --          1
    "B2"  -> Just $ "B1" + 1                -- 1 + 1 == 2
    "B3"  -> Just $ "A3" * abs "B2"         -- 3 * 2 == 6
    "C1"  -> Just $ IfZero "B3" "C2" 1000   --          1000
    "C2"  -> Just $ IfZero "B3" 2000 "C1"   --          1000
    "C3"  -> Just $ Random 1 6              --          1..6
    "F0"  -> Just 0                         --          0
    "F1"  -> Just 1                         --          1
    'F':_ -> Just $ rel (-1) 0 + rel (-2) 0 --          Fn = F(n - 1) + F(n - 2)
    _     -> Nothing

acyclicSpreadsheet :: Spreadsheet
acyclicSpreadsheet cell = case name cell of
    "B1"  -> Just 1                         --          1
    "B2"  -> Just $ "B1" + 1                -- 1 + 1 == 2
    "B3"  -> Just $ "A3" * abs "B2"         -- 3 * 2 == 6
    "C1"  -> Just $ IfZero "B3" "B2" 1000   --          1000
    "C2"  -> Just $ IfZero "B3" 2000 "C1"   --          1000
    "C3"  -> Just $ Random 1 6              --          1..6
    "F0"  -> Just 0                         --          0
    "F1"  -> Just 1                         --          1
    'F':_ -> Just $ rel (-1) 0 + rel (-2) 0 --          Fn = F(n - 1) + F(n - 2)
    _     -> Nothing

targets :: [Cell]
targets = [ "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "F0", "F1", "F4" ]

tasks :: Tasks Monad Cell Int
tasks = spreadsheetTask spreadsheet

test :: String -> Build Monad i Cell Int -> i -> IO Bool
test name build i = do
    let store   = inputs i
        result  = sequentialMultiBuild build tasks targets store
        correct = all (correctBuild tasks store result) targets
    -- when False $ putStrLn $ "========\n" ++ show (getInfo result) ++ "\n========"
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

