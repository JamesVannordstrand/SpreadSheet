module SpreadSheet(getCell, putCell, startState, Sheet, SpreadSheetType) where

import Control.Monad.State

type Sheet = [(String, String)]
type SpreadSheetType = StateT Sheet IO () 

startState :: [(String, String)]
startState = []

getCell :: String -> StateT Sheet IO String
getCell cell = get >>= \ls -> getCell' ls cell  
  where getCell' []     cell = error "Not in SpreadSheet"
        getCell' ((location, value):xs) cell
          | location == cell = return value
          | otherwise        = getCell' xs cell

putCell :: String -> String -> StateT Sheet IO ()
putCell dataa location = do
  ls <- get
  put $ putCell' ls dataa location  

putCell' :: Sheet -> String -> String -> Sheet
putCell' sheet dataa location 
  | elemSheet sheet location = replaceCell sheet dataa location
  | otherwise = appendCell sheet dataa location
  where elemSheet [] _ = False  
        elemSheet ((cell, val):xs) location 
          | location == cell = True
          | otherwise =  elemSheet xs location 
        appendCell sheet dataa location = (location, dataa) : sheet
        replaceCell sheet dataa location = map (\(cell, val) -> 
          if cell == location 
            then (location, dataa) 
            else (cell, val)) sheet 