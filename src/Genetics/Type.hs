{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Genetics.Type where

import           Control.Concurrent (MVar)
import           Data.Aeson         (ToJSON (..), object, (.=))
import           Data.Aeson.TH
import           Data.Map           (Map)

data NodeGeneType =
    Bias | Sensor | Output | Hidden
    deriving (Show, Eq, Ord)
deriveJSON defaultOptions ''NodeGeneType

data Node = Node
    { nodeNum    :: Int
    , nodeType   :: NodeGeneType
    , activation :: Double }
    deriving (Show, Eq, Ord)
deriveJSON defaultOptions ''Node

data Gene = Gene
    { geneIn      :: Int
    , geneOut     :: Int
    , geneWeight  :: Double
    , geneEnabled :: Bool
    , geneInnov   :: Int
    }
    deriving Show
deriveJSON defaultOptions 'Gene

data Genotype = Genome
    { nodes           :: [Node]
    , geneConnections :: [Gene]
    }
    deriving Show
deriveJSON defaultOptions ''Genotype

data IndividualAI = IndividualAI
    { aiId               :: Int
    , aiFitness          :: Double
    , aiCorrectedFitness :: Double
    , genome             :: Genotype
    }
    deriving Show
deriveJSON defaultOptions ''IndividualAI

data Species = Species
    { individuals :: [IndividualAI]
    , nicheId     :: Int
    , stagnant    :: Int
    , lastFitness :: Double
    }
    deriving Show
deriveJSON defaultOptions ''Species

data AIPopulation = AIPopulation
    { populationNiches :: [Species]
    , nextNicheId      :: MVar Int
    , innovations      :: MVar (Map Int (Int, Int))
    , connMap          :: MVar (Map (Int, Int) Int)
    , lastInnovationId :: MVar Int
    }
instance Show AIPopulation where
    show = show . populationNiches

instance ToJSON AIPopulation where
    toJSON p = object [ "niches" .= toJSON (populationNiches p) ]


genomeInputGenes :: Genotype -> [Node]
genomeInputGenes g = filter isInputGene (nodes g)

genomeInputConnections :: Genotype -> [Gene]
genomeInputConnections g = filter isInputConn (geneConnections g)
    where
        isInputConn x = geneIn x `elem` inputGeneNums

        inputGeneNums = map nodeNum $ genomeInputGenes g

isInputGene :: Node -> Bool
isInputGene g = nodeType g == Sensor
