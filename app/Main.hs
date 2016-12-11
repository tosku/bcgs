module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Natural
import           Grid
import           Lattice
import           Metropolis
import           System.Random

main :: IO ()
main = do
  print "give L D"
  inLD <- sequence [getLine, getLine]
  let [l , d] = map (\x -> read x :: Natural) inLD
  let n = gridN l d
  let graph = map (pbcGrid l d) [1 .. n]
  -- print graph
  let edges = pbcEdges l d
  -- print edges
  print n
  conf1 <- randomConfiguration (mkStdGen 13) n
  getEnergy <- latticeEnergy edges isingRealization conf1
  getMag <- magnetization conf1
  print $ show "Energy" ++ show getEnergy
  print $ show "Mag" ++ show getMag
  met <- metropolis getEnergy getMag tCrit l d pbcGrid (mkStdGen 13) isingRealization conf1
  print $ show "Met" ++ show met
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
