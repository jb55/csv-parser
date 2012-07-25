
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Data.CSV.Parser (
    columns
  , row
  , convert
  , convertOne
  , ColumnDef
  , FromCSV(..)
  , module Text.CSV
  ) where

import qualified Data.HashMap.Lazy as M
import           Text.CSV
import           Control.Monad.Reader
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Error

type ColumnDef = M.HashMap String Int
type CSVEnv = (ColumnDef, Record)

envRecord :: CSVEnv -> Record
envRecord = snd

envDef :: CSVEnv -> ColumnDef
envDef = fst

newtype FromCSV b a = FromCSV { fromCSV :: CSVEnv -> Either b a }

instance MonadReader CSVEnv (FromCSV b) where
  ask       = FromCSV return
  local f m = ask >>= return . f >> m

instance Functor (FromCSV b) where
  fmap f m = FromCSV $ \env -> fmap f $ fromCSV m env

instance Monad (FromCSV b) where
  return  = goodCSV
  m >>= f = FromCSV $ \env -> case fromCSV m env of
                                Left b  -> Left b
                                Right a -> fromCSV (f a) env

instance MonadError a (FromCSV a) where
  throwError = badCSV
  m `catchError` handler =
    FromCSV $ \env -> case fromCSV m env of
                        Right x -> Right x
                        Left b  -> fromCSV (handler b) env

columns :: Record -> ColumnDef
columns cols = M.fromList $ cols `zip` [0..]

badCSV :: b -> FromCSV b a
badCSV b = FromCSV $ \_ -> Left b

goodCSV :: a -> FromCSV b a
goodCSV a = FromCSV $ \_ -> Right a

row :: String -> FromCSV String String
row s = do
  (def, rec) <- (envDef &&& envRecord) <$> ask
  case M.lookup s def of
    Nothing -> badCSV $ "Missing key: " ++ s
    Just ind -> goodCSV $ rec !! ind


convertOne :: FromCSV b a -> ColumnDef -> Record -> Either b a
convertOne frcsv cd r = let f = curry . fromCSV $ frcsv
                        in f cd r

convert :: FromCSV b a -> CSV -> [Either b a]
convert _ [] = []
convert fcsv (cols:rest) =
  let cd = columns cols
  in map (convertOne fcsv cd) rest'
  where
    rest'   = filter (not . bad) rest
    bad []  = True
    bad [_] = True
    bad _   = False


