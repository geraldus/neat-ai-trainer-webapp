module XOR where

import           ClassyPrelude      (when)
import           Control.Concurrent (MVar, newMVar)
import           Data.Map           (Map, fromList)
import           Genetics.Defaults
import           Genetics.Niches    (selectNewSample)
import           Genetics.Type
import           System.Random      (randomRIO)


biasPhi :: Double
biasPhi = 0.8

basicGenome :: Double -> Double -> Double -> Genotype
basicGenome w0 w1 w2 = Genome
    { nodes =
        [ Node { nodeNum = 0, nodeType = Bias,   activation = 1.0 }
        , Node { nodeNum = 1, nodeType = Sensor, activation = 0.0 }
        , Node { nodeNum = 2, nodeType = Sensor, activation = 0.0 }
        , Node { nodeNum = 4, nodeType = Output, activation = 0.0 } ]
    , geneConnections =
        [ Gene
            { geneIn = 0
            , geneOut = 4
            , geneWeight = w0
            , geneEnabled = True
            , geneInnov = 1 }
        , Gene
            { geneIn = 1
            , geneOut = 4
            , geneWeight = w1
            , geneEnabled = True
            , geneInnov = 2 }
        , Gene
            { geneIn = 2
            , geneOut = 4
            , geneWeight = w2
            , geneEnabled = True
            , geneInnov = 3 }
        ] }

minimalInnovations :: Map Int (Int, Int)
minimalInnovations = fromList
    [ (1, (0, 4))
    , (2, (1, 4))
    , (3, (2, 4)) ]

minimalConnInnovMap :: Map (Int, Int) Int
minimalConnInnovMap = fromList
    [ ((0, 4), 1)
    , ((1, 4), 2)
    , ((2, 4), 3) ]


xorGenome :: Double -> (Double, Double) -> (Double, Double) -> Genotype
xorGenome w0 (w1, i1) (w2, i2) = Genome
    { nodes =
        [ Node { nodeNum = 0, nodeType = Bias,   activation = 1.0 }
        , Node { nodeNum = 1, nodeType = Sensor, activation = i1 }
        , Node { nodeNum = 2, nodeType = Sensor, activation = i2 }
        , Node { nodeNum = 4, nodeType = Output, activation = 0.0 } ]
    , geneConnections =
        [ Gene
            { geneIn = 0
            , geneOut = 4
            , geneWeight = w0
            , geneEnabled = True
            , geneInnov = 1 }
        , Gene
            { geneIn = 1
            , geneOut = 4
            , geneWeight = w1
            , geneEnabled = True
            , geneInnov = 2 }
        , Gene
            { geneIn = 2
            , geneOut = 4
            , geneWeight = w2
            , geneEnabled = True
            , geneInnov = 3 }
        ] }

xorGenome' :: (Double, Double) -> (Double, Double) -> Genotype
xorGenome' = xorGenome biasPhi


basicMVars :: IO (MVar Int, MVar Int, MVar Int, MVar (Map (Int, Int) Int), MVar Int)
basicMVars = do
    nextNicheMV <- newMVar 2
    nextNodeMV  <- newMVar 5
    conns       <- newMVar minimalConnInnovMap
    innId       <- newMVar (length minimalInnovations + 1)
    aiId        <- newMVar (fromIntegral populationSize + 1)
    return (nextNicheMV, aiId, nextNodeMV, conns, innId)

createBasicPopulation :: IO AIPopulation
createBasicPopulation = do
    when (populationSize < 1) $ error "Wrong population size"
    -- FIXME: Try to create species automatically
    (nextNicheMV, nextAIMV, nextNodeMV,  conns, innId) <- basicMVars
    pop <- createBasicGenotypeGroup (fromIntegral populationSize)
    let ais = map mkIndividual (zip [1 .. fromIntegral populationSize] pop)
    ns <- selectNewSample $ mkBasicSpecies ais
    return $ AIPopulation
        { populationNiches = [ns]
        , nextNicheId      = nextNicheMV
        , nextNodeId       = nextNodeMV
        , nextInnovationId = innId
        , connMap          = conns
        , nextAIIdMV       = nextAIMV
        }

mkIndividual :: (Int, Genotype) -> IndividualAI
mkIndividual (i, g) = IndividualAI
    { aiId = i
    , aiFitness = 0.0
    , aiCorrectedFitness = 0.0
    , genome = g }

mkBasicSpecies :: [IndividualAI] -> Species
mkBasicSpecies ais =
    Species
        { nicheIndividuals = ais
        , nicheId = 1
        , nicheStagnantGens = 0
        , nicheFitness = 0.0
        , nicheLastFitness = 0.0
        , nicheSample = error "Undefined niche sample"
        , nicheCorrectedFitness = 0.0
        }

createBasicGenotypeGroup :: Int -> IO [Genotype]
createBasicGenotypeGroup size = mapM (const randomAI) [1..size]
  where
    randomAI = do
        w1 <- randomRIO (-1, 1)
        w2 <- randomRIO (-1, 1)
        return $ basicGenome biasPhi w1 w2
