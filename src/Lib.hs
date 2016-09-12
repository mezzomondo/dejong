module Lib
    ( examples
    ) where

import BaseGene

import Text.Printf

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
    parseInput c

parseInput :: [Char] -> IO ()
parseInput c
    | c == "1"  = example1_1
    | c == "2"  = example1_2
    | c == "3"  = putStrLn "Three!"
    | otherwise = putStrLn "Goodbye!"