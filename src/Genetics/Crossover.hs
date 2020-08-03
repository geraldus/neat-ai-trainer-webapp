module Genetics.Crossover where

import           ClassyPrelude.Yesod (pack)
import           Data.List           (nub, sortBy)
import           Data.Ord            (comparing)
import           Genetics.Defaults   (geneDisableProbability,
                                      interspeciesMatingRate)
import           Genetics.Type
import           System.Random       (randomIO, randomRIO)


selectiveCrossover :: Int -> [Genotype] -> [Genotype] -> IO [Genotype]
selectiveCrossover n relative distant = do
    if n <= 0
        then return []
        else do
            idx <- randomRIO (0, length relative - 1)
            -- let (relInit, relTail) = splitAt idx relative
            let g = relative !! idx
            -- c <- cross g (relInit ++ drop 1 relTail) distant
            -- Чтобы избежать случаев ошибок неверного индекса, когда в нише
            -- один только экземпляр
            c <- cross g relative distant
            cs <- selectiveCrossover (n - 1) relative distant
            return $ c:cs
  where
    cross g ns os = do
        rnd <- randomRIO (0.0, 1.0)
        if rnd <= interspeciesMatingRate && not (null os)
            then do
                idx <- randomRIO (0, length os - 1)
                crossover g (os !! idx)
            else do
                idx <- randomRIO (0, length ns - 1)
                crossover g (ns !! idx)


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
