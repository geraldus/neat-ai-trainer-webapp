{-# LANGUAGE TemplateHaskell #-}
module Type.Genetics where

import           Data.Aeson.TH


data NodeGeneType =
    Bias | Sensor | Output | Hidden
    deriving (Show, Eq, Ord)
deriveJSON defaultOptions ''NodeGeneType

data Node = Node
    { nodeNum   :: Int
    , nodeType  :: NodeGeneType
    , activated :: Bool }
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
    { aiId      :: Int
    , aiFitness :: Double
    , genome    :: Genotype }
deriveJSON defaultOptions ''IndividualAI

data Species = Species { individuals :: [IndividualAI] }
deriveJSON defaultOptions ''Species

data AIPopulation = AIPopulation { populationNiches :: [Species] }
deriveJSON defaultOptions ''AIPopulation


genomeInputGenes :: Genotype -> [Node]
genomeInputGenes g = filter isInputGene (nodes g)

genomeInputConnections :: Genotype -> [Gene]
genomeInputConnections g = filter isInputConn (geneConnections g)
    where
        isInputConn x = geneIn x `elem` inputGeneNums

        inputGeneNums = map nodeNum $ genomeInputGenes g

isInputGene :: Node -> Bool
isInputGene g = nodeType g == Sensor
