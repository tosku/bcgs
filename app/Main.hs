module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Natural
import           Grid
import           Lattice
import           Metropolis
import           System.Random
import           Control.Monad.Par
import Control.Monad.Par.IO as ParIO
import Control.Monad.Trans (liftIO, MonadIO)
import System.Environment 

main :: IO ()
main = do
  print "give L D"
  print "give monte carlo steps"
  inLD <- getArgs 
  let l = read (inLD!!0) :: Natural
  let d = read (inLD!!1) :: Natural
  let mcs = read (inLD!!2) :: Int
  -- inmcs <- getLine
  -- let mcs = read inmcs :: Int
  let n = gridN l d
  -- print graph
  let edges = pbcEdges l d
  -- print edges
  print n
  let getconf = randomConfiguration (mkStdGen 13) n
  conf1 <- getconf
  let getEnergy = latticeEnergy edges isingRealization conf1
  let getMag = magnetization conf1
  en <- getEnergy
  mag <- getMag
  print $ show "Energy" ++ show en
  print $ show "Mag" ++ show mag

  
  let met seed = metropolis tCrit l d pbcGrid (mkStdGen seed) isingRealization conf1
  mets <- ParIO.runParIO $ parMapM (liftIO . met) $ take mcs (randoms (mkStdGen 51):: [Int])


  let (dEs,dMs) = unzip mets
  let energies = foldl' (\es de -> let e'= (head es) + de in e':es) [en] dEs
  let mags = foldl' (\ms dm -> let m'= (head ms) + dm in m':ms) [mag] dMs
  let esqs = map (^ 2) energies
  let magsqs = map (^ 2) mags
  print $ show "E" ++ show energies
  -- print $ show "E^2" ++ show esqs
  print $ show "M" ++ show mags
  -- print $ show "M^2" ++ show magsqs
  enn <- getEnergy
  magn <- getMag
  print $ show "newEnergy" ++ show (enn)
  print $ show "newMag" ++ show (magn )
  print "done"
  -- print b
  -- forever $ do
  --   print "flip spin?"
  --   v <- getLine
  --   flipSpin (read v :: Int) conf1
  --   print "flipped it \n"
  --   print "new conf \n"
  --   newc <- showConfiguration conf1
  --   print $ newc
  -- forever $ do
  --   print "get neighbor of "
  --   v <- getLine
  --   print "dimension"
  --   j <- getLine
  --   print $ neighbor (read v :: Vertex ) l (read j  :: D) <$> [Forward , Backward]
  --   print $ isBoundary (read v :: Vertex ) l (read j  :: D)
