module Common
    ( BaseGene(..)
    , CoupleGene(..)
    , replaceAtIndex
    , pick
    , boxMuller
    , FitnessFunction(..)
    , GeneFormat(..)
    , idIO
    , StartPopulationIO(..)
    , calcFitness
    , mutateStandardIO
    , mutateGaussIO
    , evolveIO
    , generateIO
    ) where
-- the (..) also exports the constructor

import System.Random
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

pick :: [a] -> IO a
pick xs = do
    pos <- randomRIO (0, length xs - 1)
    return (xs !! pos)

--
-- Gaussian distribution
--
boxMuller :: StdGen -> (Float, StdGen)
boxMuller gen = (sqrt (-2 * log u1) * cos (2 * pi * u2), gen'')
    where (u1, gen')  = randomR (0, 1) gen 
          (u2, gen'') = randomR (0, 1) gen'

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
-- Generic id function
--
idIO :: a -> IO a
idIO g = do
    return g

--
-- Type class with instances that generate the starting population
-- 
class StartPopulationIO g where
    startPopulationIO :: Int -> Float -> Float -> IO [g]

instance StartPopulationIO BaseGene where
    startPopulationIO n lb ub = do
        seed <- newStdGen
        return $ take n (map BaseGene (randomRs (lb, ub) seed))

instance StartPopulationIO CoupleGene where
    startPopulationIO n lb ub= do
        seed <- newStdGen
        return $ take n (map CoupleGene (zip (randomRs (lb, ub) seed) (randomRs (-5.0, 5.0) seed)))

--
-- Generic function to calculate the fitness
-- g must have a fitness function i.e. must be a gene
--
calcFitness :: FitnessFunction g => [g] -> [Float]
calcFitness xs = map fitnessFunction xs

--
-- Mutation using delta mutation (no good), only available for BaseGene
--
mutateStandardIO :: BaseGene -> IO BaseGene
mutateStandardIO g = do
    factor <- pick [-1.0, 1.0]
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
    return $ BaseGene ((getBaseGene g) + factor)

--
-- Type class with instances that probabilistically mutate a gene
-- using a gaussian distribution
--
class MutateGaussIO g where
    mutateGaussIO :: g -> IO g

instance MutateGaussIO BaseGene where
    mutateGaussIO g = do
        gen <- newStdGen
        let factor = fst $ boxMuller gen
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
        return $ BaseGene ((getBaseGene g) + factor)

instance MutateGaussIO CoupleGene where
    mutateGaussIO (CoupleGene (f, s)) = do
        gen <- newStdGen
        let (delta1, newgen) = boxMuller gen
        let delta2 = fst $ boxMuller newgen -- Ignore the new StdGen
-- making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.
        return $ CoupleGene (f + delta1, s + delta2)

--
-- Generic function to extract a gene and mutate it using 
-- a function with signature g -> IO g
--
extractElementAndMutateIO :: [g] -> Int -> (g -> IO g) -> IO g
extractElementAndMutateIO pop pos f = do
    let elem = (pop !! pos)
    offspring <- f elem
    return offspring

--
-- Generic function that takes a population and a mutation function
-- and returns a new population with the mutation in place if it has higher fitness
--
evolveIO :: FitnessFunction g => [g] -> (g -> IO g) -> Float -> Float -> IO [g]
evolveIO pop f lb ub = do
    pos <- randomRIO (0, length pop - 1)
-- select a parent randomly using a uniform probability distribution over the current population.
-- Use the selected parent to produce a single offspring
    offspring <- extractElementAndMutateIO pop pos f
-- randomly selecting a candidate for deletion from the current population using a uniform probability distribution; and keeping either the candidate or the offspring depending on wich one has higher fitness.
    pos2 <- randomRIO (0, length pop - 1)
    opponent <- extractElementAndMutateIO pop pos2 idIO
    let offFitness = fitnessFunction offspring
    let winner = if lb <= offFitness && offFitness <= ub
            then if offFitness >= (fitnessFunction opponent)
                then offspring
                else opponent
            else opponent
    --let winner = if (fitnessFunction opponent) >= (fitnessFunction offspring) then opponent else offspring
    return (replaceAtIndex pos2 winner pop)

--
-- Core generic function that generates n generations starting fom
-- the initial population using a probabilistic mutation function
-- [g] starting population
-- (g -> IO g) mutation function
-- [[g]] accumulator
-- n number of steps remaining
-- IO [[g]] full history
--
generateIO :: FitnessFunction g => [g] -> (g -> IO g) -> [[g]] -> Int -> Float -> Float -> IO [[g]]
generateIO xs f acc n lb ub =
    if n == 0
        then do
            ys <- evolveIO xs f lb ub
            return ([ys]++[xs]++acc)  
        else do
            ys <- evolveIO xs f lb ub
            generateIO ys f ([xs]++acc) (n-1) lb ub