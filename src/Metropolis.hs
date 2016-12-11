module Metropolis
    ( metropolis
    , tCrit
    ) where

import           Control.Monad (foldM, liftM, replicateM)
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
  δM <- fmap (spinToInt . not) $ getSpinSTM v conf
  δE <- negate . (* 2.0) . sum <$> mapM (\e -> edgeEnergySTM e real conf) (grid l d v)
  if (δE <= 0.0) then flipit
  else if ( exp (- δE / temp)< prop) then flipit
    else return ()
  return (δE,δM)
  where flipit = flipSpinSTM v conf

metropolisStep :: Temperature -> L -> D -> Grid -> StdGen -> Realization -> Configuration -> IO (StdGen,Energy,Int)
metropolisStep temp l d grid seed real conf = do
  let (p, g') = randomR (0.0, 1.0) seed :: (Double, StdGen)
  let (v, g'') = randomR (1, V.length conf) g'
  -- print (p,v,grid l d v)
  (de,dm) <- atomically $ metropolisFlipSTM temp p v l d grid real conf
  return (g'',de,dm)

metropolis :: Energy -> Int -> Temperature -> L -> D -> Grid -> StdGen -> Realization -> Configuration -> IO (Energy,Int)
metropolis energy mag temp l d grid seed real conf = do
  a <- ((return (seed,0.0,0)) >>= sumdeltas) <$> (ms seed)
  print a
  return (energy,mag)
  where iterations = gridN l d
        ms g = metropolisStep temp l d grid g real conf
        sumdeltas = (\(g,de,dm) -> return (g, ((+):: Double) de, ((+):: Int) dm))
