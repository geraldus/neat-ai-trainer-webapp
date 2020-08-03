module Genetics.Defaults where


-- * Population

populationCoefficient :: Int
populationCoefficient = 1

populationSize :: Integer
populationSize = fromIntegral populationCoefficient * 150

-- * NEAT Parameters

c1 :: Double
c1 = 1.0

c2 :: Double
c2 = 1.0

c3 :: Double
c3 = 0.4


deltaT :: Double
deltaT = 3.0

delta :: Double -> Double -> Double -> Double -> Double
delta d e n w = c1*e/n + c2*d/n + c3*w


stagnantGenerationsLimit :: Int
stagnantGenerationsLimit = 15


weightMutationProbability :: Double
weightMutationProbability = 0.8

randomWeightMutationProbability :: Double
randomWeightMutationProbability = 0.1

popMutationFactor :: Double
popMutationFactor = 0.25


-- | Maximum difference in weights when uniform perturbation applied.
--   Value was chosen randomly because there were no example value
--   in docs.
perturbationFactor :: Double
perturbationFactor = 0.1

geneDisableProbability :: Double
geneDisableProbability = 0.75

interspeciesMatingRate :: Double
interspeciesMatingRate = 0.0001

smNodeAdditionProbability :: Double
smNodeAdditionProbability = 0.03

smLinkAdditionProbability :: Double
smLinkAdditionProbability = 0.05

lgLinkAdditionProbability :: Double
lgLinkAdditionProbability = 0.3


solutionTimeout :: Int
solutionTimeout = 1 * 1000 * 1000
