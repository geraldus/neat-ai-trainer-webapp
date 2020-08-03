module Genetics.Niches where

import           Control.Concurrent (MVar, putMVar, takeMVar)
import           Data.List          (findIndex, sortBy)
import           Data.Maybe         (catMaybes)
import           Data.Ord           (comparing)
import           Genetics.Defaults
import           Genetics.Type
import           System.Random      (Random (randomRIO))


  where
    l = length inds


addToNiche ::
    MVar Int -> [Species] -> IndividualAI -> IO [Species]
addToNiche mv ns i = do
    let idx' = findNicheIndex ns i
    case idx' of
        Just idx -> do
            -- putStrLn "ADD to niche"
            let n    = ns !! idx
                list = nicheIndividuals n
                f    = nicheFitness n
                f'   = nicheCorrectedFitness n
                n'   = n { nicheIndividuals      = i:list
                         , nicheFitness          = f + aiFitness i
                         , nicheCorrectedFitness = f' + aiCorrectedFitness i
                         }
                (nHead, _:nTail) = splitAt idx ns
            return $ (nHead ++ [n'] ++ nTail)
        Nothing -> do
            nId <- takeMVar mv
            putMVar mv (nId + 1)
            let n' = Species
                        { nicheId               = nId
                        , nicheSample           = i
                        , nicheIndividuals      = [i]
                        , nicheLastFitness      = 0.0
                        , nicheFitness          = aiFitness i
                        , nicheStagnantGens     = 0
                        , nicheCorrectedFitness = aiCorrectedFitness i
                        }
            return (ns ++ [n'])


champions :: [Species] -> ([IndividualAI], [Species])
champions ns = (champs, specs)
  where
    niches = map sortNiche $ filter largeEnough ns

    mayChampions = map takeChampion niches

    (champs', specs) = unzip mayChampions

    champs = catMaybes champs'

    largeEnough = (> 5) . length . individuals

takeChampion :: Species -> (Maybe IndividualAI, Species)
takeChampion s = (ch, s { nicheIndividuals = inds})
  where
   (ch, inds) = maybeFirstIndividual s

maybeFirstIndividual :: Species -> (Maybe IndividualAI, [IndividualAI])
maybeFirstIndividual s = case nicheIndividuals s of
    []        -> (Nothing, [])
    (ai:rest) -> (Just ai, rest)

sortNiche :: Species -> Species
sortNiche s =
    s { nicheIndividuals = sortByFitness (nicheIndividuals s) }

bothOfSameNiche :: Genotype -> Genotype -> Bool
bothOfSameNiche g1 g2 = dlt < deltaT
  where
    (ms, djs, exs) = crossoverMap g1 g2
    e  = length exs
    d  = length djs
    maxGenes = max (genesCount g1) (genesCount g2)
    n  = if maxGenes < 20
        then 1
        else maxGenes
    w = avgWeightDiff ms
    dlt = delta (fromIntegral d) (fromIntegral e) (fromIntegral n) w


-- * Utils

distributeFitness :: AIPopulation -> AIPopulation
distributeFitness p = p { populationNiches = ns' }
  where
    ns = populationNiches p

    pop = concatMap nicheIndividuals ns

    ns' = map (distributeNiche pop) ns

distributeNiche :: [IndividualAI] -> Species -> Species
distributeNiche pop n = n { nicheIndividuals = corrected }
  where
    cfs = map (\i -> correctedFitness i pop) (nicheIndividuals n)

    corrected = map
        (\(f, i) -> i { aiCorrectedFitness = f })
        (zip cfs (nicheIndividuals n))


correctedFitness :: IndividualAI -> [IndividualAI] -> Double
correctedFitness ind specs = aiFitness ind / (sameSpeciesSum ind specs)

sameSpeciesSum :: IndividualAI -> [IndividualAI] -> Double
sameSpeciesSum ind = sum . map (shDelta (genome ind) . genome)

shDelta :: Genotype -> Genotype -> Double
shDelta g1 g2 = if dist > deltaT then 0 else dist
  where
    n = fromIntegral $ max (genesCount g1) (genesCount g2)
    (m', d', e') = crossoverMap g1 g2
    d = fromIntegral $ length d'
    e = fromIntegral $ length e'
    w = avgWeightDiff m'
    dist = delta d e n w

avgWeightDiff :: [(Gene, Gene)] -> Double
avgWeightDiff ms = sum diffs / (fromIntegral $ length diffs)
  where
    diffs = map (\(g1, g2) -> abs (geneWeight g1 - geneWeight g2)) ms

genesCount :: Genotype -> Int
genesCount = length . geneConnections


-- * Misc (for test purposes)

-- ai1 = Genome [] t1
-- ai2 = Genome [] t2

-- t1 = [ Gene 0 4 0 True 0
--      , Gene 1 4 0 False 1
--      , Gene 2 4 0 False 2
--      , Gene 1 5 1 True 3
--      , Gene 5 4 0 True 4
--      , Gene 2 6 1 True 6
--      , Gene 6 3 0 True 7
--      , Gene 3 5 0 True 9
--      ]

-- t2 = [ Gene 0 4 0 True 0
--      , Gene 1 4 0 False 1
--      , Gene 2 4 0 False 2
--      , Gene 1 5 1 True 3
--      , Gene 5 4 0 True 4
--      , Gene 2 5 0 True 5
--      , Gene 2 6 1 True 6
--      , Gene 6 3 0 True 7
--      , Gene 2 6 0 True 8
--      ]
