#!/usr/bin/env nix-shell
#!nix-shell -i runghc
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Control.Monad.Primitive
import qualified Data.ByteString.Lazy            as LBS
import           Data.Csv
import           Data.Foldable                   (traverse_)
import qualified Data.List                       as List
import           Data.Monoid
import qualified Data.Vector                     as Vector
import           GHC.Generics
import           Prelude                         hiding (readFile)
import           System.IO
import           System.Random.MWC
import           System.Random.MWC.Distributions

data MinMax = MinMax
  { _min :: Double
  , _max :: Double
  } deriving stock (Show)

data Sign = Minus | Plus
  deriving stock (Enum)

instance Show Sign where
  show Plus  = "(+)"
  show Minus = "(-)"

-- | At most one factor.
amo :: [Sign] -> Bool
amo = (<= 1). getSum . foldMap (Sum . fromEnum)


-- The Double represents the median of all replications of that combinations of factors.
type Table = [([Sign], Double)]

getTableValues :: Table -> [Double]
getTableValues = fmap snd

-- The Double represents the affect on the answer of the given combination of factors.
type YatesResult = [([Sign], Double)]

-- (take 10 $ repeat $ MinMax maxBound minBound)
toMinMax :: [[Double]] -> [MinMax]
toMinMax xss =
  foldr go [] (List.transpose xss)
  where
    go xs acc = let x = MinMax (minimum xs) (maximum xs) in x:acc


-- The return are the first column of Yates i.e. the mean of each combination.
factorialDesign :: [MinMax] -> IO Table
factorialDesign minMaxs = do
  gen <- createSystemRandom
  go gen minMaxs [] []
  where
    go :: GenIO -> [MinMax] -> [Sign] -> [Double] -> IO Table
    go gen [] ss vs =
      (\r -> [(ss, r)]) <$> computeAnswer gen vs
    go gen (f:fs) ss vs =
      liftA2 (++)
             (go gen fs (ss ++ [Minus]) (vs ++ [_min f]))
             (go gen fs (ss ++ [Plus])  (vs ++ [_max f]))


    computeAnswer :: GenIO -> [Double] -> IO Double
    computeAnswer gen xs = do
      let formula  = 0.005562 + 1.032*(xs !! 0) + 0.988*(xs !! 1) + 0.988*(xs !! 3) + 4.94*(xs !! 4)
          -- We add some noise.
          computeFormula =  (formula +) <$> uniformR (0.0, 1.0) gen
      mean <$> replicateM 20 computeFormula

    mean :: (Foldable t, Fractional a) => t a -> a
    mean xs = sum xs / fromIntegral (length xs)

-- Yates: returns the effect on the answer of each factor.
yates :: Table -> YatesResult
yates xs =
  zip (fst <$> xs)
      (go (length xs + 1) (snd <$> xs))
  where
    go :: Int -> [Double] -> [Double]
    go 0 ys = let l = fromIntegral $ length xs
               in (head ys / l) : ((/ (l/2)) <$> tail ys)
    go n ys = go (n - 1) (add2 ys ++ subtract2 ys)

    add2 :: Num a => [a] -> [a]
    add2 []             = []
    add2 (x1 : x2 : xs) = (x1 + x2) : add2 xs

    subtract2 :: Num a => [a] -> [a]
    subtract2 []             = []
    subtract2 (x1 : x2 : xs) = (x2 - x1) : subtract2 xs

saveModelData :: FilePath -> Table -> IO ()
saveModelData fp table =
  let modelData = Only <$> getTableValues table
   in LBS.writeFile fp (encode modelData)

writeResults :: FilePath -> [([Sign], Double)] -> IO ()
writeResults fp result =
  withFile fp WriteMode (\h -> traverse_ (writeRow h) result)
  where
    writeRow h (ss, result) = hPutStrLn h $ show ss ++ "  =  " ++ show result

main :: IO ()
main = do
  putStrLn "=== Factorial design started ==="
  bs      <- LBS.readFile "../ex1/dataset.csv"
  putStrLn "- Read dataset"
  dataset <- either fail (pure . fmap init . Vector.toList) $ decode @[Double] HasHeader bs
  putStrLn "- Decode dataset"
  table <- factorialDesign $ toMinMax dataset
  putStrLn "- Data and Experiment prepared"
  saveModelData "model_data.csv" table
  putStrLn "- Data saved"
  let result = yates table
  writeResults "yates.txt" result
  putStrLn "- Yates result saved"
  writeResults "yates_single_factor.txt" (filter (amo . fst) result)
  putStrLn "- Yates single factor saved"
  putStrLn "=== Successfuly completed ==="
