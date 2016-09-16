module BaseGene
    ( BaseGene
    , baseFitnessFunction
    , baseStartPopulationIO
    , baseCalcFitness
    , mutateStandardIO
    , baseMutateGaussIO
    , generateIO
    ) where

import System.Random
import Data.List

import Common

type History = [[BaseGene]]

baseFitnessFunction :: BaseGene -> Float
baseFitnessFunction = \x -> 50 - (x^2)

baseStartPopulationIO :: Int -> IO [BaseGene]
baseStartPopulationIO n = do
    seed <- newStdGen
    return $ startPopulation n seed

baseCalcFitness :: [BaseGene] -> [Float]
baseCalcFitness xs = map baseFitnessFunction xs

mutateStandardIO :: BaseGene -> IO BaseGene
mutateStandardIO g = do
    factor <- pick [-1.0, 1.0]
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    return (g + factor)

baseMutateGaussIO :: BaseGene -> IO BaseGene
baseMutateGaussIO g = do
    gen <- newStdGen
    let factor = fst $ boxMuller gen
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    return (g + factor)

extractRandomElementIO :: [BaseGene] -> Int -> (BaseGene -> IO BaseGene) -> IO BaseGene
extractRandomElementIO pop pos f = do
    let elem = (pop !! pos)
    offspring <- f elem
    return offspring

evolveIO :: [BaseGene] -> (BaseGene -> IO BaseGene) -> IO [BaseGene]
evolveIO pop f = do
    pos <- randomRIO (0, length pop - 1)
-- select a parent randomly using a uniform probability distribution over the current population.
-- Use the selected parent to produce a single offspring
    offspring <- extractRandomElementIO pop pos f
-- randomly selecting a candidate for deletion from the current population using a uniform probability distribution; and keeping either the candidate or the offspring depending on wich one has higher fitness.
    pos2 <- randomRIO (0, length pop - 1)
    opponent <- extractRandomElementIO pop pos2 idIO
    let winner = if (baseFitnessFunction opponent) >= (baseFitnessFunction offspring) then opponent else offspring
    return (replaceAtIndex pos2 winner pop)

generateIO :: [BaseGene] -> (BaseGene -> IO BaseGene) -> History -> Int -> IO History
generateIO xs f acc n =
    if n == 0
        then do
           ys <- evolveIO xs f
           return ([ys]++[xs]++acc)  
        else do
            ys <- evolveIO xs f
            generateIO ys f ([xs]++acc) (n-1)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls