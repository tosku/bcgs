module Lattice
    ( randomConfiguration
    , getSpin
    , flipSpin
    , spinToInt
    , showConfiguration
    , flipConfiguration
    , edgeEnergySTM
    , edgeEnergy
    , latticeEnergy
    , isingRealization
    , magnetization
    , Realization
    , Configuration
    , Energy
    , Magnetization
    , getSpinSTM
    , flipSpinSTM
    ) where

import           Control.Concurrent     ()
import           Control.Concurrent.STM
import           Control.Monad          (liftM,foldM)
import           Data.Natural
import qualified Data.Vector            as V
import           Grid
import           System.Random

type Spin = Bool -- probably more memory efficient
type Configuration = V.Vector (TVar Spin)
type Energy = Double
type J = Double -- Exchange interaction
type Realization = Edge -> J

isingRealization :: Realization
isingRealization _ = 1;

edgeEnergySTM :: Edge -> Realization -> Configuration -> STM Energy
edgeEnergySTM e real conf = do
  let (s,t) = e
  spinSource <- fmap (fromIntegral . spinToInt) (getSpinSTM s conf)
  spinTarget <- fmap (fromIntegral . spinToInt) (getSpinSTM t conf)
  return $! - spinSource * spinTarget * real e

edgeEnergy :: Edge -> Realization -> Configuration -> IO Energy
edgeEnergy e real conf = atomically $ edgeEnergySTM e real conf

latticeEnergy :: [Edge] -> Realization -> Configuration -> IO Energy
latticeEnergy edges real conf = do
  a <- mapM (\e -> edgeEnergy e real conf) edges
  let b = sum a
  return $! b

type Magnetization = Int
magnetizationSTM :: Configuration -> STM Magnetization
magnetizationSTM = V.foldM (\x s -> (fmap ((+ x) . spinToInt) . readTVar) s) 0
magnetization :: Configuration -> IO Magnetization
magnetization conf = atomically $ magnetizationSTM conf


randomConfigurationSTM :: (RandomGen g) => g -> Int -> STM Configuration
randomConfigurationSTM g x = V.mapM newTVar (V.fromList $ take x $ randoms g)
randomConfiguration :: (RandomGen g) => g -> Int -> IO Configuration
randomConfiguration g x = atomically $ randomConfigurationSTM g x

getSpinSTM :: Vertex -> Configuration -> STM Spin
getSpinSTM v conf = readTVar $ conf V.! (v-1)

getSpin :: Vertex -> Configuration -> IO Spin
getSpin v conf = atomically $ getSpinSTM v conf

flipSpinSTM :: Vertex -> Configuration -> STM ()
flipSpinSTM v conf = do
  s <- getSpinSTM v conf
  writeTVar (conf V.! (v - 1)) $ not s

flipSpin :: Vertex -> Configuration -> IO ()
flipSpin v conf = atomically $ flipSpinSTM v conf

showConfiguration :: Configuration -> IO [Int]
showConfiguration c = mapM (\v -> spinToInt <$> getSpin v c) [1 .. (length c)]

flipConfiguration :: Configuration -> IO [()]
flipConfiguration c = mapM (`flipSpin` c) [1 .. (length c)]

spinToInt :: Spin -> Int
spinToInt s = if s then 1 else -1
