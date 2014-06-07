module Main where

import Control.Monad.State (lift, runStateT)
import SpreadSheet

main :: IO ()
main = runStateT startSpreadSheet startState >>= print

startSpreadSheet :: SpreadSheetType
startSpreadSheet = do
  lift $ print "Get or Put"
  choice <- lift getLine
  if choice == "Get" 
    then getFromSheet
    else addToSheet
  startSpreadSheet

getFromSheet :: SpreadSheetType
getFromSheet = do
  lift $ print "What cell"
  lift getLine >>= \val -> getCell val >>= lift . print

addToSheet :: SpreadSheetType
addToSheet = do
  lift $ print "Enter the data "
  dataa <- lift getLine
  lift $ print "Enter the location "
  location <- lift getLine
  putCell dataa location