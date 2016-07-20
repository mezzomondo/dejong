module Lib
    ( examples
    ) where

import System.Random
import Data.List
import Text.Printf

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

format :: (Text.Printf.PrintfArg a, Text.Printf.PrintfArg b) => (a, b) -> IO ()
format x = putStrLn ("Gene: " ++ (printf "%12.8f" (fst x) ++ " " ++ "; Fitness: " ++ (printf "%14.8f" (snd x))))

example1 :: (Gene -> IO Gene) -> String -> IO ()
example1 f s = do
    let limit = 1000
    let popsize = 10
-- Randomly generate the initial population of M individuals (using a uniform probability distribution over the entire geno/phenospace) and compute the fitness of each individual.    
    pop <- startPopulationIO popsize 
    newpop <- generateIO pop f [] limit 
    putStrLn ("Simulation limit (#births): " ++ show limit)
    putStrLn "Fintness function: y = 50 - (x^2)"
    putStrLn "Bounds: [-100.0, 100.0]"
    putStrLn s
    putStrLn ("Population size: " ++ show popsize)
    putStrLn "First generation:"
    mapM_ format (zip pop (calcFitness pop))
    let sec = newpop !! 990
    putStrLn "Second generation:"
    mapM_ format (zip sec (calcFitness sec))
    let sixth = newpop !! 950
    putStrLn "Sixth generation:"
    mapM_ format (zip sixth (map fitnessFunction sixth))
    let last = head newpop
    putStrLn "Last generation:"
    mapM_ format (zip last (map fitnessFunction last))
    examples

example1_1 :: IO()
example1_1 = example1 mutateStandardIO "Using delta mutation with step size 1.0"

example1_2 :: IO ()
example1_2 = example1 mutateGaussIO "Using gaussian mutation with step size 1.0"

examples :: IO ()
examples = do
    putStrLn "Examples available:"
    putStrLn "1) Chapter one example one (delta mutation step size 1.0)."
    putStrLn "2) Chapter one example two (gaussian mutation mean 0, standard deviation 1.0)."
    putStrLn "Make your choice (q to quit):"
    c <- getLine
    if c == "1"
        then example1_1
        else if c == "2"
            then example1_2
            else putStrLn "Goodbye!"