## First three examples of first chapter of 'Evolutionary Computation a unified approach' by Kenneth A. De Jong written in Haskell

Here's the algorithm as stated in the book (p. 4):

```
EV:
 Randomly generate the initial population of M individuals (using a uniform probability distribution over
 the entire geno/phenospace) and compute the fitness of each individual.
 Do Forever:
   Choose a parent as follows:

     - select a parent randomly using a uniform probability distribution over the current population.

   Use the selected parent to produce a single offspring by:
     
     - making an identical copy of the parent, and then probabilistically mutating it to produce the offspring.

   Compute the fitness of the offspring.

   Select a member of the population to die by:

     - randomly selecting a candidate for deletion from the current population using a uniform probability
       distribution; and keeping either the candidate or the offspring depending on wich one has
       higher fitness.
 End Do
```

I choose this example in order to learn some "real world" Haskell (and I found it very challenging), comments and suggestions are welcome.

## Instructions

```
$ stack build
$ stack exec dejong-exe
```
