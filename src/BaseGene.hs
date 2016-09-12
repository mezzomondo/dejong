module BaseGene
    ( Gene
    , fitnessFunction
    , startPopulationIO
    , calcFitness
    , mutateStandardIO
    , mutateGaussIO
    , generateIO
    ) where

import System.Random
import Data.List

type Gene = Float
type History = [[Gene]]

fitnessFunction :: Gene -> Float
fitnessFunction = \x -> 50 - (x^2)

startPopulation :: RandomGen g => Int -> g -> [Gene]
startPopulation n seed = take n (randomRs (-100.0, 100.0) seed :: [Gene])

startPopulationIO :: Int -> IO [Gene]
startPopulationIO n = do
    seed <- newStdGen
    return $ startPopulation n seed

calcFitness :: [Gene] -> [Float]
calcFitness xs = map fitnessFunction xs

mutateStandardIO :: Gene -> IO Gene
mutateStandardIO g = do
    factor <- pick [-1.0, 1.0]
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    return (g + factor)

boxMuller :: StdGen -> Float
boxMuller gen = sqrt (-2 * log u1) * cos (2 * pi * u2)
    where (u1, gen')  = randomR (0, 1) gen 
          (u2, _) = randomR (0, 1) gen'

mutateGaussIO :: Gene -> IO Gene
mutateGaussIO g = do
    gen <- newStdGen
    let factor = boxMuller gen
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    return (g + factor)

idIO :: Gene -> IO Gene
idIO g = do
    return g

extractRandomElementIO :: [Gene] -> Int -> (Gene -> IO Gene) -> IO Gene
extractRandomElementIO pop pos f = do
    let elem = (pop !! pos)
    offspring <- f elem
    return offspring

evolveIO :: [Gene] -> (Gene -> IO Gene) -> IO [Gene]
evolveIO pop f = do
    pos <- randomRIO (0, length pop - 1)
-- select a parent randomly using a uniform probability distribution over the current population.
-- Use the selected parent to produce a single offspring
    offspring <- extractRandomElementIO pop pos f
-- randomly selecting a candidate for deletion from the current population using a uniform probability distribution; and keeping either the candidate or the offspring depending on wich one has higher fitness.
    pos2 <- randomRIO (0, length pop - 1)
    opponent <- extractRandomElementIO pop pos2 idIO
    let winner = if (fitnessFunction opponent) >= (fitnessFunction offspring) then opponent else offspring
    return (replaceAtIndex pos2 winner pop)

generateIO :: [Gene] -> (Gene -> IO Gene) -> History -> Int -> IO History
generateIO xs f acc n =
    if n == 0
        then do
           ys <- evolveIO xs f
           return ([ys]++[xs]++acc)  
        else do
            ys <- evolveIO xs f
            generateIO ys f ([xs]++acc) (n-1)

pick :: [a] -> IO a
pick xs = do
    pos <- randomRIO (0, length xs - 1)
    return (xs !! pos)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls