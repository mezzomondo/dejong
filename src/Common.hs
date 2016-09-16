module Common
    ( BaseGene
    , CoupleGene(..)
    , pick
    , boxMuller
    , idIO
    , startPopulation
    ) where
-- the (..) also exports the constructor

import System.Random

type BaseGene = Float
newtype CoupleGene = CoupleGene { getCoupleGene :: (Float, Float) } deriving Show

pick :: [a] -> IO a
pick xs = do
    pos <- randomRIO (0, length xs - 1)
    return (xs !! pos)

boxMuller :: StdGen -> (Float, StdGen)
boxMuller gen = (sqrt (-2 * log u1) * cos (2 * pi * u2), gen'')
    where (u1, gen')  = randomR (0, 1) gen 
          (u2, gen'') = randomR (0, 1) gen'

class IdIO g where
    idIO :: g -> IO g

instance IdIO Float where
    idIO g = do
        return g

instance IdIO CoupleGene where
    idIO g = do
        return g

class StartPopulation g where
    startPopulation :: RandomGen r => Int -> r -> [g]

instance StartPopulation Float where
    startPopulation n seed = take n (randomRs (-100.0, 100.0) seed :: [BaseGene])

instance StartPopulation CoupleGene where
    startPopulation n seed = take n (map CoupleGene (zip (randomRs (-5.0, 5.0) seed) (randomRs (-5.0, 5.0) seed)))