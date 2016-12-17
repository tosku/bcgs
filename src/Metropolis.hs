module Metropolis
    ( metropolis
    , tCrit
    ) where

import           Control.Monad (foldM, liftM, replicateM, (<=<),(>=>))
import qualified Data.Vector   as V
import           Grid
import           Lattice
import           System.Random
import           Control.Concurrent.STM

type Temperature = Double

tCrit :: Temperature --2D square Ising
tCrit = 2.0 * j / log (1.0 + sqrt 2.0) where j = 1

metropolisFlipSTM :: Temperature -> Double -> Vertex -> L -> D -> Grid -> Realization -> Configuration -> STM (Energy,Int)
metropolisFlipSTM temp prop v l d grid real conf = do
  δM <- ((* 2) . spinToInt . not) <$> getSpinSTM v conf
  δE <- negate . (* 2.0) . sum <$> mapM (\e -> edgeEnergySTM e real conf) (grid l d v)
  if (δE <= 0.0) || ( exp (- δE / temp)< prop) then
    (do flipit
        return (δE, δM))
    else return (0.0, 0)
  where flipit = flipSpinSTM v conf

metropolisStep :: Temperature -> L -> D -> Grid -> StdGen -> Realization -> Configuration -> IO (StdGen,Energy,Int)
metropolisStep temp l d grid seed real conf = do
  let (p, g') = randomR (0.0, 1.0) seed :: (Double, StdGen)
  let (v, g'') = randomR (1, V.length conf) g'
  (de,dm) <- atomically $ metropolisFlipSTM temp p v l d grid real conf
  return (g'',de,dm)

--Since metropolis is local no need for global energy or magnetization to be fed
metropolis :: Temperature -> L -> D -> Grid -> StdGen -> Realization -> Configuration -> IO (Energy,Int)
metropolis temp l d grid seed real conf = do
  (g,de,dm) <- let applyMS 0 = bindMS
                   applyMS iters = bindMS >=> applyMS (iters - 1)
                in applyMS iterations (seed, 0.0, 0)
  return (de,dm)
  where iterations = gridN l d
        ms g = metropolisStep temp l d grid g real conf
        bindMS (g,de,dm) = do (g',de',dm') <- ms g
                              return (g',de+de',dm+dm')
