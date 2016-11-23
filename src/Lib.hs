module Lib
    ( examples
    ) where

import Common
import System.Random
import Control.Monad.State.Lazy

baseExample :: (InBounds g, FitnessFunction g, GeneFormat g) => StdGen -> (Int -> Float -> Float -> State StdGen [g]) -> (g -> State StdGen g) -> Float -> Float -> String -> String -> IO ()
baseExample seed f g lb ub s ff = do
    let limit = 1000
    let popsize = 10
-- Randomly generate the initial population of M individuals (using a uniform probability distribution over the entire geno/phenospace) and compute the fitness of each individual.
    let (pop, newseed) = runState (f popsize lb ub) seed
    let newpop = evalState (generate lb ub pop [] limit g) newseed
    putStrLn ("Simulation limit (#births): " ++ show limit)
    putStrLn ("Fintness function: " ++ ff)
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
example1_1 = do
    seed <- newStdGen
    baseExample seed (startPopulation :: Int -> Float -> Float -> State StdGen [BaseGene])(mutateStandard :: BaseGene -> State StdGen BaseGene) (-100.0) 100.0 "BaseGene - Using delta mutation with step size 1.0" "50 - (x^2)"

example1_2 :: IO ()
example1_2 = do
    seed <- newStdGen
    baseExample seed (startPopulation :: Int -> Float -> Float -> State StdGen [BaseGene])(mutateGauss :: BaseGene -> State StdGen BaseGene) (-100.0) 100.0 "BaseGene - Using gaussian mutation with step size 1.0" "50 - (x^2)"

example1_3 :: IO ()
example1_3 = do
    seed <- newStdGen
    baseExample seed (startPopulation :: Int -> Float -> Float -> State StdGen [CoupleGene])(mutateGauss :: CoupleGene -> State StdGen CoupleGene) (-5.0) 5.0 "CoupleGene - Using gaussian mutation with step size 1.0" "x^2 + y^2"