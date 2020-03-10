#!/usr/bin/env nix-shell
#!nix-shell -i runghc

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Csv                        as Csv
import           Data.Vector.Generic             (Vector)
import qualified Data.Vector.Generic             as GV
import qualified Data.Vector.Unboxed             as UV
import           System.Random.MWC
import           System.Random.MWC.Distributions as MWCD
import           Control.Monad (forM_)
import           GHC.IO.Handle
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as C
import qualified System.IO                       as IO
import           Data.String.Conv (toS, toSL)


stdDev :: Double
stdDev = 1.0

main = do
  IO.withFile "normal.csv" IO.WriteMode $
    \handle -> do
      vss <- traverse (normalV 10000) [0.0, 0.0, 10.0]
      forM_ vss $ \vs ->
        let bs = Csv.encode [GV.toList vs]
        in LBS.hPut handle bs
  where
    normalV n mean =
      withSystemRandom $
        \(gen::GenST s) -> normalVector mean stdDev gen n :: ST s (UV.Vector Double)

-------------------------

normalVector :: (PrimMonad m, Vector v Double)
             => Double            -- ^ Mean
             -> Double            -- ^ Standard deviation
             -> Gen (PrimState m)
             -> Int               -- ^ vector length
             -> m (v Double)
normalVector mean std gen n =
  GV.replicateM n (MWCD.normal mean std gen)

standardVector :: (PrimMonad m, Vector v Double)
             => Gen (PrimState m)
             -> Int               -- ^ vector length
             -> m (v Double)
standardVector = normalVector 0.0 1.0
