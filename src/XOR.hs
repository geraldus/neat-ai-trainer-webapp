module XOR where

import           System.Random (randomRIO)
import           Type.Genetics

biasPhi :: Double
biasPhi = 0.8

basicGenome :: Double -> Double -> Double -> Genotype
basicGenome w0 w1 w2 = Genome
    { nodes =
        [ Node { nodeNum = 0, nodeType = Bias,   activated = True }
        , Node { nodeNum = 1, nodeType = Sensor, activated = False }
        , Node { nodeNum = 2, nodeType = Sensor, activated = False }
        , Node { nodeNum = 4, nodeType = Output, activated = False } ]
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

xorGenome :: Double -> (Double, Bool) -> (Double, Bool) -> Genotype
xorGenome w0 (w1, i1) (w2, i2) = Genome
    { nodes =
        [ Node { nodeNum = 0, nodeType = Bias,   activated = True }
        , Node { nodeNum = 1, nodeType = Sensor, activated = i1 }
        , Node { nodeNum = 2, nodeType = Sensor, activated = i2 }
        , Node { nodeNum = 4, nodeType = Output, activated = False } ]
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


createPopulation ::  Int -> IO [Genotype]
createPopulation popSize = mapM (const randomAI) [1..popSize]
  where
    randomAI = do
        w1 <- randomRIO (-1, 1)
        w2 <- randomRIO (-1, 1)
        return $ basicGenome biasPhi w1 w2
