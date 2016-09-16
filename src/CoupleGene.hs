module CoupleGene
    ( coupleFitnessFunction
    , coupleStartPopulationIO
    ) where

import System.Random

import Common

type History = [[CoupleGene]] 

coupleFitnessFunction :: CoupleGene -> Float
coupleFitnessFunction = \(x, y) -> x^2 + y^2

coupleStartPopulationIO :: Int -> IO [CoupleGene]
coupleStartPopulationIO n = do
    seed <- newStdGen
    return $ startPopulation n seed

-- coupleCalcFitness :: [CoupleGene] -> [Float]
-- coupleCalcFitness xs = map coupleFitnessFunction xs

--coupleMutateGaussIO :: CoupleGene -> IO CoupleGene
--coupleMutateGaussIO (f, s) = do
--    gen <- newStdGen
--    let (delta1, newgen) = boxMuller gen
--    let delta2 = fst $ boxMuller newgen -- Ignore the new StdGen
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
--    return (f + delta1, s + delta2)

-- idIO :: CoupleGene -> IO CoupleGene
-- idIO g = do
--    return g