module Genetics.Crossover where

import           Data.List         (nub, sortBy)
import           Data.Ord          (comparing)
import           Genetics.Defaults (geneDisableProbability)
import           Genetics.Type
import           System.Random     (randomIO, randomRIO)


crossover :: Genotype -> Genotype -> IO Genotype
crossover p1 p2 = do
    let (m, d, e) = crossoverMap p1 p2
    m' <- mapM selectRandomGene m
    let cons = sortBy (comparing geneInnov) $ m' ++ d ++ e
    let ns = nub $  matchConnNodes cons (nodes p1 ++ nodes p2)
    return $ Genome ns cons

selectRandomGene :: (Gene, Gene) -> IO Gene
selectRandomGene (c1, c2) = do
    r <- randomIO :: IO Bool
    let g = if r then c1 else c2
    g' <- if not (geneEnabled c1 && geneEnabled c2)
        then do
            dr <- randomRIO (0.0, 1.0)
            return $ g { geneEnabled = dr > geneDisableProbability }
        else return g
    return g'

matchConnNodes :: [Gene] -> [Node] -> [Node]
matchConnNodes cs ns = filter (\n -> nodeNum n `elem` targetIds) ns
  where
    outNodeIds = map geneOut cs
    inNodeIds  = map geneIn  cs
    targetIds  = nub $ outNodeIds ++ inNodeIds

crossoverMap :: Genotype -> Genotype -> ([(Gene, Gene)], [Gene], [Gene])
crossoverMap g1 g2 = crossovers
                        (geneConnections g1)
                        (geneConnections g2)
                        ([], [], [])


crossovers :: [Gene]
           -> [Gene]
           -> ([(Gene, Gene)], [Gene], [Gene])
           -> ([(Gene, Gene)], [Gene], [Gene])
crossovers [] [] acc = acc
crossovers ns [] (ms, djs, exs) = (ms, djs, reverse ns ++ exs)
crossovers [] ns (ms, djs, exs) = (ms, djs, reverse ns ++ exs)
crossovers (g1:g1s) (g2:g2s) (ms, djs, exs)
    | geneInnov g1 == geneInnov g2 = crossovers g1s g2s ((g1, g2):ms, djs, exs)
    | geneInnov g1 <  geneInnov g2 =
        if null g1s
            then (ms, djs, reverse g2s ++ g2:g1:exs)
            else crossovers g1s (g2:g2s) (ms, g1:djs, exs)
    | geneInnov g1 >  geneInnov g2 =
        if null g2s
            then (ms, djs, reverse g1s ++ g1:g2:exs)
            else crossovers (g1:g1s) g2s (ms, g2:djs, exs)
    | otherwise = error "Seems not to be a case"
