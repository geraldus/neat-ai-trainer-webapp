module Genetics.Niches where

import           ClassyPrelude.Yesod (pack)
import           Control.Concurrent  (MVar, putMVar, takeMVar)
import           Control.Monad       (when)
import           Data.List           (findIndex, sortBy)
import           Data.Map            (Map)
import           Data.Maybe          (catMaybes)
import           Data.Ord            (comparing)
import           Genetics.Crossover
import           Genetics.Defaults
import           Genetics.Mutations
import           Genetics.Type
import           System.Random       (Random (randomRIO))
import           Text.Pretty.Simple


crossoverNiche :: MVar Int
               -> MVar Int
               -> MVar (Map (Int, Int) Int)
               -> MVar Int
               -> Double
               -> [Genotype]
               -> Species
               -> IO [IndividualAI]
crossoverNiche nodeMV innovMV connInnovMV aiIdMV fPop otherPopSpecies n =
    if nicheStagnantGens n >= stagnantGenerationsLimit
        then return []
        else do
            let ais = nicheIndividuals n
            let popSize = length otherPopSpecies + length ais
            let (chmp, rest) = if length ais > championNicheMinCount
                then takeChampions' 5 ais
                else ([], ais)
            let cleanedRest = if largeEnoughForChampion (length ais)
                    then removeWeakest rest
                    else rest
            weightMutatedGenomes <- mapM mutateWeights' (map genome cleanedRest)
            let rest' = zipWith
                    (\ai g -> ai { genome = g })
                    cleanedRest
                    weightMutatedGenomes
            let fTot = nicheCorrectedFitness n / fromIntegral (length (nicheIndividuals n))
            let allOffsprings = allottedOffsprings fPop fTot (fromIntegral popSize)
            let allotted = max 0 $ allOffsprings - length chmp
            wMut <- mapM mWeight' rest'
            sMut' <- mapM mStruct' wMut
            let sMut = concatRight sMut'
                pars = chmp ++ concatLeft sMut'
                -- Это обход крайнего случая, когда нет кандидатур для выбора
                -- родителя.  Нужно отдельно рассмотреть эту ситуацию
                pars' = if null pars then wMut else pars
            let allotted' = max 0 $ allotted - length sMut
            offs  <- selectiveCrossover
                        allotted' (map genome pars') otherPopSpecies
            offs' <- mapM toAI offs
            return (chmp ++ offs' ++ sMut)
  where
    mWeight' ai = do
        rnd <- randomRIO (0.0, 1.0)
        if rnd > weightMutationProbability
            then return ai
            else do
                m <- mutateWeights' (genome ai)
                return $ ai { genome = m }

    mStruct' ai = do
        rnd <- randomRIO (0.0, 1.0)
        if rnd > popMutationFactor
            then return $ Left ai
            else do
                m <- mutateStruct nodeMV innovMV connInnovMV (genome ai)
                return $ Right ai { genome = m }

    toAI g = do
        nextId <- takeMVar aiIdMV
        putMVar aiIdMV (nextId + 1)
        return $ IndividualAI
            { aiId = nextId
            , aiFitness = 0
            , aiCorrectedFitness = 0
            , genome = g }

    concatLeft = concatMap concl
      where
        concl (Left x) = [x]
        concl _        = []

    concatRight = concatMap concr
      where
        concr (Right x) = [x]
        concr _         = []

    removeWeakest ais =
        let avg    = sum (map aiFitness ais) / fromIntegral (length ais)
        in filter (\ai -> aiFitness ai > avg) ais

selectNewSample :: Species -> IO Species
selectNewSample s = do
    let individuals = nicheIndividuals s
    if null individuals
        then pure s
        else do
            idx <- randomRIO (0, length individuals - 1)
            return $ s { nicheSample = individuals !! idx }

recreateNiches :: MVar Int -> [IndividualAI] -> [Species] -> IO [Species]
recreateNiches mv ais ns = recLoop ais niches'
  where
    niches' = map clone ns

    clone n = n { nicheIndividuals      = []
                , nicheFitness          = 0.0
                , nicheLastFitness      = nicheFitness n
                , nicheCorrectedFitness = 0.0
                }

    recLoop []     niches = return niches
    recLoop (i:is) niches = addToNiche mv niches i >>= recLoop is

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

    largeEnough = largeEnoughForChampion . length . nicheIndividuals

takeChampions' :: Int -> [IndividualAI] -> ([IndividualAI], [IndividualAI])
takeChampions' n s = (take n sorted, drop n sorted)
  where
    sorted = sortByFitness s


findChampion :: Species -> (Maybe IndividualAI, Species)
findChampion n = (fst (takeChampion sorted), sorted)
  where
    sorted = sortNiche n

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

findNicheIndex :: [Species] -> IndividualAI -> Maybe Int
findNicheIndex ns i = findIndex matchingNiche ns
  where matchingNiche = bothOfSameNiche (genome i) . genome . nicheSample

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


sortByFitness :: [IndividualAI] -> [IndividualAI]
sortByFitness = sortBy (flip (comparing aiFitness))

correctedFitness :: IndividualAI -> [IndividualAI] -> Double
correctedFitness ind specs = aiFitness ind / (sameSpeciesSum ind specs)

sameSpeciesSum :: IndividualAI -> [IndividualAI] -> Double
sameSpeciesSum ind = sum . map (shDelta (genome ind) . genome)

allottedOffsprings :: (RealFrac a, Integral b) => a -> a -> a -> b
allottedOffsprings fPop fTot p = ceiling $ p * fTot / fPop

largeEnoughForChampion :: Int -> Bool
largeEnoughForChampion = (> championNicheMinCount)

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
