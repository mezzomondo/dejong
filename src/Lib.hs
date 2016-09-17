module Lib
    ( examples
    ) where

import Common

example1 :: (StartPopulationIO g, FitnessFunction g, GeneFormat g) => (g -> IO g) -> Float -> Float -> String -> IO ()
example1 f lb ub s = do
    let limit = 1000
    let popsize = 10
-- Randomly generate the initial population of M individuals (using a uniform probability distribution over the entire geno/phenospace) and compute the fitness of each individual.    
    pop <- startPopulationIO popsize lb ub
    newpop <- generateIO pop f [] limit lb ub
    putStrLn ("Simulation limit (#births): " ++ show limit)
    putStrLn "Fintness function: y = 50 - (x^2)"
    putStrLn ("Bounds: [" ++ show lb ++ ", " ++ show ub ++ "]")
    putStrLn s
    putStrLn ("Population size: " ++ show popsize)
    putStrLn "First generation:"
    mapM_ geneFormat (zip pop (calcFitness pop))
    let ten = newpop !! 810
    putStrLn "Tenth generation:"
    mapM_ geneFormat (zip ten (calcFitness ten))
    let fifty = newpop !! 510
    putStrLn "Fiftyth generation:"
    mapM_ geneFormat (zip fifty (map fitnessFunction fifty))
    let last = head newpop
    putStrLn "Last generation:"
    mapM_ geneFormat (zip last (map fitnessFunction last))
    examples

examples :: IO ()
examples = do
    putStrLn "Examples available:"
    putStrLn "1) Chapter one example one (base gene, delta mutation step size 1.0)."
    putStrLn "2) Chapter one example two (base gene, gaussian mutation mean 0, standard deviation 1.0)."
    putStrLn "3) Chapter one example three (couple gene, gaussian mutation mean 0, standard deviation 1.0)."
    putStrLn "Make your choice (q to quit):"
    c <- getLine
    parseInput c

parseInput :: [Char] -> IO ()
parseInput c
    | c == "1"  = example1_1
    | c == "2"  = example1_2
    | c == "3"  = example1_3
    | otherwise = putStrLn "Goodbye!"

example1_1 :: IO()
example1_1 = example1 (mutateStandardIO :: BaseGene -> IO BaseGene) (-100.0) 100.0 "BaseGene - Using delta mutation with step size 1.0"

example1_2 :: IO ()
example1_2 = example1 (mutateGaussIO :: BaseGene -> IO BaseGene) (-100.0) 100.0 "BaseGene - Using gaussian mutation with step size 1.0"

example1_3 :: IO ()
example1_3 = example1 (mutateGaussIO :: CoupleGene -> IO CoupleGene) (-5.0) 5.0 "CoupleGene - Using gaussian mutation with step size 1.0"
--example1_3 :: IO ()
--example1_3 = example1 (mutateStandardIO :: CoupleGene -> IO CoupleGene) (-5.0) 5.0 "CoupleGene - Using gaussian mutation with step size 1.0"