#!/usr/bin/env nix-shell
#!nix-shell -i runghc
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where

import           Data.ByteString.Builder
import           Data.ByteString.Lazy
import           Data.Csv
import           GHC.Generics
import           Prelude                         hiding (writeFile)
import           System.Random.MWC
import           System.Random.MWC.Distributions

data Individual = Individual
  { factor1  :: Double
  , factor2  :: Double
  , factor3  :: Double
  , factor4  :: Double
  , factor5  :: Double
  , factor6  :: Double
  , factor7  :: Double
  , factor8  :: Double
  , factor9  :: Double
  , factor10 :: Double
  , answer   :: Double
  } deriving stock (Show, Generic)
    deriving anyclass (ToRecord, ToNamedRecord, DefaultOrdered)

generateIndividual :: IO Individual
generateIndividual = withSystemRandom . asGenIO $ \gen -> do
  f1 <- abs <$> standard gen
  f2 <- abs <$> uniform gen
  f3 <- abs <$> exponential 0.5 gen
  f4 <- abs <$> standard gen
  f5 <- abs <$> uniform gen
  let f6 = f1 + f2
      f7 = f1 + 2*f3
      f8 = f1 + f4
      f9 = f4 + 5*f5
      f10 = f1 + f2 + f3 + f4 + f5
  answer <- (f1 + f2 + f9 +) <$> standard gen
  return (Individual f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 answer)

generateDataset :: Int -> IO [Individual]
generateDataset 0 = return []
generateDataset n = do
  x  <- generateIndividual
  xs <- generateDataset (n - 1)
  return (x : xs)

main :: IO ()
main = (writeFile "dataset.csv" . encodeDefaultOrderedByName) =<< generateDataset 2000
