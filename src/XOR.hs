module XOR where

import           Control.Concurrent (newMVar)
import           Genetics.Defaults
import           Genetics.Type
import           System.Random      (randomRIO)
import Data.Map (Map, fromList)


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

createPopulation :: IO AIPopulation
createPopulation = do
    pop   <- createBasicGenotypeGroup (fromIntegral populationSize)
    mv    <- newMVar 2
    innov <- newMVar minimalInnovations
    conns <- newMVar minimalConnInnovMap
    innId <- newMVar (length minimalInnovations + 1)
    -- FIXME: Split group into niches
    let ais = map mkIndividual (zip [1 .. fromIntegral populationSize] pop)
    return $ AIPopulation
        { populationNiches = [Species { individuals = ais, nicheId = 1 }]
        , nextNicheId = mv
        , innovations = innov
        , connMap = conns
        , lastInnovationId = innId }

mkIndividual :: (Int, Genotype) -> IndividualAI
mkIndividual (i, g) = IndividualAI
    { aiId = i
    , aiFitness = 0.0
    , aiCorrectedFitness = 0.0
    , genome = g }

createBasicGenotypeGroup ::  Int -> IO [Genotype]
createBasicGenotypeGroup popSize = mapM (const randomAI) [1..popSize]
  where
    randomAI = do
        w1 <- randomRIO (-1, 1)
        w2 <- randomRIO (-1, 1)
        return $ basicGenome biasPhi w1 w2