
module Control.CSV (
    columns
  , rowVal
  , rowValM
  , convert
  , ColumnDef
  , module Text.CSV
  ) where

import qualified Data.HashMap.Lazy as M
import Text.CSV
import Data.Either.Utils (forceEither)

type ColumnDef = M.HashMap String Int

parseCSV' = parseCSV ","

columns :: Record -> ColumnDef
columns cols = M.fromList $ cols `zip` [0..]


rowValM :: ColumnDef -> Record -> String -> Either String String
rowValM def r s = case M.lookup s def of
  Nothing  -> Left $ "Missing key: " ++ s
  Just ind -> Right $ r !! ind


rowVal :: ColumnDef -> Record -> String -> String
rowVal def r s =
  let rvm = rowValM def r s
  in forceEither rvm

convert :: (ColumnDef -> Record -> a) -> [Record] -> [a]
convert _ [] = []
convert f (cols:rest) =
  let colDef = columns cols
  in map (f colDef) rest'
  where
    rest'   = filter (not . bad) rest
    bad []  = True
    bad [x] = True
    bad _   = False


