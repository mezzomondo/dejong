module Common
    ( BaseGene(..)
    , CoupleGene(..)
    , replaceAtIndex
    , boxMuller
    , InBounds
    , FitnessFunction(..)
    , GeneFormat(..)
    , StartPopulation(..)
    , calcFitness
    , mutateStandard
    , mutateGauss
    , evolve
    , generate
    ) where
-- the (..) also exports the constructor

import System.Random
import Control.Monad.State.Lazy
import Text.Printf

--
-- Our genes
--
newtype BaseGene = BaseGene { getBaseGene :: Float } deriving Show
newtype CoupleGene = CoupleGene { getCoupleGene :: (Float, Float) } deriving Show

-- 
-- Some basic tools
--
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

pick :: [a] -> StdGen -> (a, StdGen)
pick xs seed = extractInCouple xs (getPosition xs seed)
    
-- Helpers for the pick function 
bounds :: [a] -> (Int, Int)
bounds xs = (0, length xs - 1)

getPosition :: [a] -> StdGen -> (Int, StdGen)
getPosition xs g = randomR (bounds xs) g

extractInCouple :: [a] -> (Int, StdGen) -> (a, StdGen)
extractInCouple xs (i, g) = (xs !! i, g)
--
-- Gaussian distribution
--
boxMuller :: StdGen -> (Float, StdGen)
boxMuller seed = (sqrt (-2 * log u1) * cos (2 * pi * u2), seed_2)
    where (u1, seed_1)  = randomR (0, 1) seed 
          (u2, seed_2) = randomR (0, 1) seed_1

--
-- Check if in bounds
--
class InBounds g where
    inBounds :: g -> Float -> Float -> Bool

instance InBounds BaseGene where
    inBounds (BaseGene g) lb ub = (lb <= g) && (g <= ub) 

instance InBounds CoupleGene where
    inBounds (CoupleGene (x, y)) lb ub = (lb <= x) && (x <= ub) && (lb <= y) && (y <= ub)

--
-- Fitness functions (type class)
-- In the end from Haskell point of view a gene is something
-- that has a fitness function
--
class FitnessFunction g where
    fitnessFunction :: g -> Float

instance FitnessFunction BaseGene where
    fitnessFunction = \(BaseGene x) -> 50 - (x^2)

instance FitnessFunction CoupleGene where
    fitnessFunction = \(CoupleGene (x, y)) -> x^2 + y^2

-- 
-- Print formatting functions (type class)
--
class GeneFormat g where
    geneFormat :: (g, Float) -> IO ()

instance GeneFormat BaseGene where
    geneFormat x = do
        let g = getBaseGene $ fst x
        let f = snd x
        putStrLn ("Gene: " ++ (printf "%12.8f" g ++ " " ++ "; Fitness: " ++ (printf "%14.8f" f)))

instance GeneFormat CoupleGene where
    geneFormat x = do
        let (g1, g2) = getCoupleGene $ fst x
        let f = snd x
        putStrLn ("Gene: (" ++ (printf "%12.8f" g1) ++ ", " ++ (printf "%12.8f" g2) ++"); Fitness: " ++ (printf "%14.8f" f))

--
-- Type class with instances that generate the starting population
-- 
class StartPopulation g where
    startPopulation :: Int -> Float -> Float -> State StdGen [g]

instance StartPopulation BaseGene where
    -- startPopulation n lb ub seed = take n $ map BaseGene (randomRs (lb, ub) seed)
    startPopulation n lb ub = do
        replicateM n $ fmap BaseGene $ state $ randomR (lb, ub)

instance StartPopulation CoupleGene where
    -- startPopulation n lb ub seed_1 = take n $ map CoupleGene $ zip (randomRs (lb, ub) seed_1) (randomRs (lb, ub) seed_2)
    --    where (_, seed_2) = random seed_1 :: (Float, StdGen)
    startPopulation n lb ub = do
        replicateM n $ fmap CoupleGene $ state $ randomR' (lb, ub)

-- These functions helped me to sort out wrapping/unwrapping in the State Monad
-- randomR'' :: (Float, Float) -> State StdGen BaseGene
-- randomR'' (lb, ub) = fmap BaseGene (state (randomR (lb, ub)))

-- randoms' :: Int -> Float -> Float -> State StdGen [BaseGene]
-- randoms' n lb ub = replicateM n (randomR'' (lb, ub))

-- randoms :: Int -> Float -> Float -> StdGen -> ([BaseGene], StdGen)
-- randoms n lb ub = runState (randoms' n lb ub)

-- randomState :: Float -> Float -> State StdGen CoupleGene
-- randomState lb ub = fmap CoupleGene (state (randomR' (lb, ub)))

-- This is actually used in startPopulation
randomR' :: (Float, Float) -> StdGen -> ((Float, Float), StdGen)
randomR' (lb, ub) seed = ((fst (randomR (lb, ub) seed), fst (randomR (lb, ub) seed_1)), seed_2)
    where (seed_1, seed_2) = split seed

--
-- Generic function to calculate the fitness
-- g must have a fitness function i.e. must be a gene
--
calcFitness :: FitnessFunction g => [g] -> [Float]
calcFitness xs = map fitnessFunction xs

--
-- Mutation using delta mutation
--
class MutateStandard g where
    mutateStandard :: g -> State StdGen g

instance MutateStandard BaseGene where
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    mutateStandard (BaseGene g) = do
        seed <- get
        let (factor, newseed) = pick [-1.0, 1.0] seed
        put newseed
        return (BaseGene (g + factor))

instance MutateStandard CoupleGene where
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    mutateStandard (CoupleGene (f, s)) = do
        seed <- get
        let (factor_1, newseed) = pick [-1.0, 1.0] seed
        let (factor_2, finalseed) = pick [-1.0, 1.0] newseed
        put finalseed
        return (CoupleGene (f + factor_1, s + factor_2))

--
-- Type class with instances that probabilistically mutate a gene
-- using a gaussian distribution
--
class MutateGauss g where
    mutateGauss :: g -> State StdGen g

instance MutateGauss BaseGene where
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    mutateGauss (BaseGene g) = do
        seed <- get
        let (factor, newseed) = boxMuller seed
        put newseed
        return (BaseGene (g + factor))

instance MutateGauss CoupleGene where
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    mutateGauss (CoupleGene (f, s)) = do
        seed <- get
        let (factor_1, newseed) = boxMuller seed
        let (factor_2, finalseed) = boxMuller newseed
        put finalseed
        return (CoupleGene (f + factor_1, s + factor_2))
--
-- Generic function to extract a gene and mutate it using 
-- a function with signature g -> g
--
extractElementAndMutate :: [g] -> Int -> (g -> State StdGen g) -> State StdGen g
extractElementAndMutate pop pos f = do
    seed <- get
    let elem = pop !! pos
    let (offspring, newseed) = runState (f elem) seed
    put newseed
    return offspring

--
-- Generic function that takes a population and a mutation function
-- and returns a new population with the mutation in place if it has higher fitness
--
evolve :: (InBounds g, FitnessFunction g) => Float -> Float -> [g] -> (g -> State StdGen g) -> State StdGen [g]
evolve lb ub pop f = do
    seed <- get
    let (pos_1, seed_1) = randomR (bounds pop) seed
    let (pos_2, seed_2) = randomR (bounds pop) seed_1
-- select a parent randomly using a uniform probability distribution over the current population.
-- Use the selected parent to produce a single offspring
    let (offspring, finalseed) = runState (extractElementAndMutate pop pos_1 f) seed_2
    let opponent = pop !! pos_2
    let offFitness = fitnessFunction offspring
    let oppFitness = fitnessFunction opponent
    let winner = if offFitness > oppFitness then offspring else opponent
    put finalseed
    if inBounds offspring lb ub
-- randomly selecting a candidate for deletion from the current population using a uniform probability distribution;
-- and keeping either the candidate or the offspring depending on wich one has higher fitness.
        then return (replaceAtIndex pos_2 winner pop)
        else return pop 
 
--
-- Core generic function that generates n generations starting fom
-- the initial population using a probabilistic mutation function
-- [g] starting population
-- (g -> State StdGen g) mutation function
-- [[g]] accumulator
-- n number of steps remaining
-- IO [[g]] full history
--
generate :: (InBounds g, FitnessFunction g) => Float -> Float -> [g] -> [[g]] -> Int -> (g -> State StdGen g) -> State StdGen [[g]]
generate lb ub xs acc n f =
    if n == 0
        then do
            ys <- evolve lb ub xs f
            return ([ys]++[xs]++acc)  
        else do
            ys <- evolve lb ub xs f
            generate lb ub ys ([xs]++acc) (n-1) f