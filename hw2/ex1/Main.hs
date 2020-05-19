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
  { unFactor1  :: Double
  , unFactor2  :: Double
  , unFactor3  :: Double
  , unFactor4  :: Double
  , unFactor5  :: Double
  , unFactor6  :: Double
  , unFactor7  :: Double
  , unFactor8  :: Double
  , unFactor9  :: Double
  , unFactor10 :: Double
  , unAnswer   :: Double
  } deriving stock (Show, Generic)
    deriving anyclass (ToRecord)

generateIndividual :: IO Individual
generateIndividual = withSystemRandom . asGenIO $ \gen -> do
  f1 <- standard gen
  f2 <- uniform gen
  f3 <- exponential 0.5 gen
  f4 <- standard gen
  f5 <- uniform gen
  let f6 = f1 + f2
      f7 = f1 + f3*2
      f8 = f3 - f2
      f9 = f1 + f2 + f3
      f10 = f1 + f2 - f3
  answer <- (*) (f1 + 2*f2 - f6 + f7 + 3*f8 + f9 + f10) <$> standard gen
  return (Individual f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 answer)

generateDataset :: Int -> IO [Individual]
generateDataset 0 = return []
generateDataset n = do
  x  <- generateIndividual
  xs <- generateDataset (n - 1)
  return (x : xs)

main :: IO ()
main = (writeFile "dataset.csv" . encode) =<< generateDataset 2000
