module Lib
    ( one_one 
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
    return $ take n (randomRs (-100.0, 100.0) seed :: [Gene])

calcFitness :: [Gene] -> [Float]
calcFitness xs = map fitnessFunction xs

mutateIO :: Gene -> IO Gene
mutateIO g = do
    factor <- pick [-1.0, 1.0]
    return (g + (factor * 0.1))

evolveIO :: [Gene] -> IO [Gene]
evolveIO pop = do
    pos <- randomRIO (0, length pop - 1)
    let elem = (pop !! pos)
    offspring <- mutateIO elem
    pos2 <- randomRIO (0, length pop - 1)
    let opponent = (pop !! pos2)
    let winner = if (fitnessFunction opponent) >= (fitnessFunction offspring) then opponent else offspring
    return (replaceAtIndex pos2 winner pop)

generateIO :: [Gene] -> History -> Int -> IO History
generateIO xs acc n =
    if n == 0
        then do
           ys <- evolveIO xs
           return ([ys]++[xs]++acc)  
        else do
            ys <- evolveIO xs
            generateIO ys ([xs]++acc) (n-1)

pick :: [a] -> IO a
pick xs = do
    pos <- randomRIO (0, length xs - 1)
    return (xs !! pos)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

one_one :: IO ()
one_one = do
    let limit = 1000
    let popsize = 10
    pop <- startPopulationIO popsize 
    newpop <- generateIO pop [] limit 
    putStrLn ("Simulation limit (#births): " ++ show limit)
    putStrLn "Fintness function: y = 50 - (x^2)"
    putStrLn "Bounds: [-100.0, 100.0]"
    putStrLn "Using delta mutation with step size 0.1"
    putStrLn ("Population size: " ++ show popsize)
    putStrLn "First generation (with fitness):"
    print (zip pop (map fitnessFunction pop))
    let sec = newpop !! 990
    putStrLn "Second generation (with fitness):"
    print (zip sec (map fitnessFunction sec))
    let sixty = newpop !! 930
    putStrLn "Sixth generation (with fitness):"
    print (zip sixty (map fitnessFunction sixty))
    let last = head newpop
    putStrLn "Last generation (with fitness):"
    print (zip last (map fitnessFunction last))