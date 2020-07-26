module Genetics.Defaults where


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
delta e d n w = c1*e/n + c2*d/n + c3*w

stagnantGenerationsLimit :: Integer
stagnantGenerationsLimit = 15
