module Genetics.Niches where

import           Control.Concurrent (MVar, putMVar, takeMVar)
import           Data.List          (findIndex, sortBy)
import           Data.Maybe         (catMaybes)
import           Data.Ord           (comparing)
import           Genetics.Defaults
import           Genetics.Type
import           System.Random      (Random (randomRIO))


nicheEmptyMap :: Species -> IO NicheRep
nicheEmptyMap (Species inds nichedId _ _) = do
    i <- randomRIO (1, l)
    let rep = inds !! (i - 1)
    return $ NicheRep
        { nRepId = nichedId
        , nRepSample = genome rep
        , nRepList = []
        , nRepFitness = 0 }
  where
    l = length inds

separateByNiche :: MVar Int -> [IndividualAI] -> [NicheRep] -> IO [NicheRep]
separateByNiche _  []     nrs = return nrs
separateByNiche mv (i:is) nrs = do
    (rep', _) <- addToNiche mv nrs i
    separateByNiche mv is rep'

addToNiche :: MVar Int -> [NicheRep] -> IndividualAI -> IO ([NicheRep], NicheRep)
addToNiche mv ns i = do
    let idx' = findNicheRepIndex ns i
    case idx' of
        Just idx -> do
            let n    = ns !! idx
                list = nRepList n
                l    = fromIntegral $ length list
                f    = nRepFitness n
                n' = n { nRepList = i:list
                       , nRepFitness = (f * l + aiCorrectedFitness i) / (l + 1)
                       }
                (nHead, _:nTail) = splitAt idx ns
            return $ (nHead ++ [n'] ++ nTail, n')
        Nothing -> do
            nId <- takeMVar mv
            putMVar mv (nId + 1)
            let n' = NicheRep
                        { nRepId      = nId
                        , nRepSample  = genome i
                        , nRepList    = [i]
                        , nRepFitness = aiCorrectedFitness i
                        }
            return (ns ++ [n'], n')

data NicheRep = NicheRep
    { nRepId      :: Int
    , nRepSample  :: Genotype
    , nRepList    :: [IndividualAI]
    , nRepFitness :: Double
    }
    deriving Show



champions :: [Species] -> ([IndividualAI], [Species])
champions ns = (champs, specs)
  where
    niches = map sortNiche $ filter largeEnough ns

    mayChampions = map takeChampion niches

    (champs', specs) = unzip mayChampions

    champs = catMaybes champs'

    largeEnough = (> 5) . length . individuals

takeChampion :: Species -> (Maybe IndividualAI, Species)
takeChampion s = (ch, s { individuals = inds})
  where
   (ch, inds) = nicheChampion s

nicheChampion :: Species -> (Maybe IndividualAI, [IndividualAI])
nicheChampion s = case individuals s of
    []        -> (Nothing, [])
    (ai:rest) -> (Just ai, rest)

sortNiche :: Species -> Species
sortNiche s = s { individuals = sortBy (flip (comparing aiFitness)) (individuals s) }

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

findNicheRepIndex :: [NicheRep] -> IndividualAI -> Maybe Int
findNicheRepIndex ns i =
    findIndex (\n -> bothOfSameNiche (genome i) (nRepSample n)) ns


-- | Crossovers and Offsprings

crossoverMap :: Genotype -> Genotype -> ([(Gene, Gene)], [Gene], [Gene])
crossoverMap g1 g2 = crossovers
                        (geneConnections g1)
                        (geneConnections g2)
                        ([], [], [])


crossovers :: [Gene] -> [Gene] -> ([(Gene, Gene)], [Gene], [Gene]) -> ([(Gene, Gene)], [Gene], [Gene])
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


-- nicheOffsprings n pop =

distributeFitness :: AIPopulation -> AIPopulation
distributeFitness p = p { populationNiches = ns' }
  where
    ns = populationNiches p

    pop = concatMap individuals ns

    ns' = map (distributeNiche pop) ns


distributeNiche :: [IndividualAI] -> Species -> Species
distributeNiche pop n = n { individuals = corrected }
  where
    cfs = map (\i -> correctedFitness i pop) (individuals n)

    corrected = map
        (\(f, i) -> i { aiCorrectedFitness = f })
        (zip cfs (individuals n))


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
