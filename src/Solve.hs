{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Solve where

import           ClassyPrelude.Yesod
import           Control.Concurrent  (threadDelay)
import           Data.List           (foldl)
import           Genetics.Type


-- * Constants

solutionTimeout :: Int
solutionTimeout = 1 * 1000 * 1000

populationCoefficient :: Int
populationCoefficient = 1

populationSize :: Integer
populationSize = fromIntegral populationCoefficient * 150


-- * Solution

runNiche :: TChan Value -> Species -> IO [Double]
runNiche ch = mapM (runXORSpecies ch) . individuals

runXORSpecies :: TChan Value -> IndividualAI -> IO Double
runXORSpecies ch ai = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar
    _ <- concurrently
        (concurrently
            (runSpeciesInput m1 ai (0, 0))
            (runSpeciesInput m2 ai (1, 0)))
        (concurrently
            (runSpeciesInput m3 ai (0, 1))
            (runSpeciesInput m4 ai (1, 1)))
    r1' <- readMVar m1
    r2' <- readMVar m2
    r3' <- readMVar m3
    r4' <- readMVar m4
    let r1 = head' r1'
        r2 = head' r2'
        r3 = head' r3'
        r4 = head' r4'
    let results = [r1, r2, r3, r4]
    let expect = [0.0, 1.0, 1.0, 0.0]
    let matrix = zip results expect
    let f   = (4 - (sum . fitness $ matrix))**2
    atomically $ writeTChan ch (object [ "results" .= toJSON [r1', r2', r3', r4']
                                       , "fitness" .= f
                                       ])
    return f
  where
    head' []    = error "Expected non empty state"
    head' (x:_) = x

    fitness rs = map f' rs

    f' ((Nothing, _), _) = 1.0
    f' ((Just [], _), _) = 1.0
    f' ((Just ((_, v):_), _), e) = abs $ e - v


runSpeciesInput ::
    MVar [(Maybe [(Node, Double)], Genotype)] -> IndividualAI -> (Double, Double) -> IO ()
runSpeciesInput mv ai inp = do
    let g = setAIInput inp (genome ai)
    putMVar mv [(Nothing, g)]
    race_
        (threadDelay solutionTimeout)
        (feedForwardLoop mv)

feedForwardLoop :: MonadIO m => MVar [(Maybe [(Node, Double)], Genotype)] -> m ()
feedForwardLoop mv = do
    state <- readMVar mv
    case state of
        [] -> error "Assuming non-empty state"
        ((done, s):_) -> do
            case done of
                Just _ -> return ()
                Nothing -> do
                    s' <- feedForward s
                    _ <- swapMVar mv (s':state)
                    return ()

feedForward :: Monad m => Genotype -> m (Maybe [(Node, Double)], Genotype)
feedForward g = do
    let (respondents, _, _) = signalActivations g
    let g' = deactivateSANodes g respondents
    if haveUnspreadSignal g'
        then return (Nothing, g')
        else return (Just (filter isOutput respondents), g')
  where
    isOutput (Node _ Output _, _) = True
    isOutput _                    = False


haveUnspreadSignal :: Genotype -> Bool
haveUnspreadSignal g =
    let active = map nodeNum (genomeActiveNonBiasNodes g)
    in any (\c -> geneIn c `elem` active) (geneConnections g)

-- | Returns a list of node activations with list of active connections
--   depending on current signal spread in neuron nodes.  In other words
--   this function returns next state of nodes during signal spread or
--   this is a reaction of neurons for current signal state.
signalActivations :: Genotype -> ([(Node, Double)], [Node], [[Gene]])
signalActivations g = (candidateActivations, activeNodes, activeCons)
  where
    cons'                      = geneConnections g
    activeNodes                = genomeActiveNodes g
    (activeNodeCons, _)        = groupNodeCons isConnInput activeNodes cons' True
    (_, activeCons)            = unzip activeNodeCons
    (candidateNodeTriggers, _) = groupNodeCons isConnTrigger (nodes g) (concat activeCons) False
    candidateVectors           = map (findTriggerValues activeNodes) candidateNodeTriggers
    candidateActivations        = map react candidateVectors
    react (n, iv, wv)      = (n, (sigmoidTransfer' $ inputWeightProduct iv wv))

findTriggerValues :: [Node] -> (a, [Gene]) -> (a, [Double], [Double])
findTriggerValues vals (n, cons') = (n, weights, inputs)
  where
    weights = map geneWeight cons'

    inputs  = map findInputVal cons'

    findInputVal c = maybe 0.0 activation $ find (flip isConnInput c) vals

deactivateSANodes :: Genotype -> [(Node, Double)] -> Genotype
deactivateSANodes g keep = g { nodes = nodes' }
  where
    nodes' = map update' (nodes g)
    update' n =
        let f = find (\(x, _) -> nodeNum x == nodeNum n) keep
        in case (f, n) of
            -- bias'es are always activated
            (_, (Node _ Bias _)) -> n {activation = 1.0 }
            -- deactivate other nodes if no reaction found
            (Nothing, _)         -> n { activation = 0.0 }
            -- otherwise set node state corresponding to its reaction
            (Just (_, s), _)     -> n { activation = s }


setAIInput :: (Double, Double) -> Genotype -> Genotype
setAIInput (i1, i2) g = g { nodes = nodes' }
  where
    nodes' = flip map (nodes g) $ \n ->
        case nodeNum n of
        1 -> n { activation = i1 }
        2 -> n { activation = i2 }
        _ -> n


-- | Generic function to fold node with associated genes by a given predicate.
--   Returns result of grouping paired with unused connections (genes).
groupNodeCons ::
    (Node -> Gene -> Bool) -> [Node] -> [Gene] -> Bool -> ([(Node, [Gene])], [Gene])
groupNodeCons cond' ns cons' allowEmpty = foldl step ([], cons') ns
  where
    step (acc, cs) n =
        let (nCons, rest) = partition (cond' n) cs
        in if null nCons && not allowEmpty
           then (acc, rest)
           else ((n, nCons):acc, rest)

genomeActiveNodes :: Genotype -> [Node]
genomeActiveNodes g = filter isAnyActiveNode (nodes g)

genomeActiveNonBiasNodes :: Genotype -> [Node]
genomeActiveNonBiasNodes g = filter isNonBiasActiveNode (nodes g)


isBiasConn :: Node -> Gene -> Bool
isBiasConn (Node n Bias _) c = geneIn c == n
isBiasConn _ _               = False

isAnyActiveNode :: Node -> Bool
isAnyActiveNode (Node _ _ a) = a /= 0

isNonBiasActiveNode :: Node -> Bool
isNonBiasActiveNode (Node _ Bias _) = False
isNonBiasActiveNode (Node _ _ a)    = a /= 0


isConnInput ::  Node -> Gene -> Bool
isConnInput n с = geneIn с == nodeNum n

isConnTrigger :: Node -> Gene -> Bool
isConnTrigger n c = geneOut c == nodeNum n


-- * Activation Functions

heavySideStep :: Double -> Bool
heavySideStep = (>= 0)

sigmoidTransfer' :: Floating a => a -> a
sigmoidTransfer' x = 1 / (1 + (exp 1)**(-4.9*x))


-- * Math Utils

inputWeightProduct :: [Double] -> [Double] -> Double
inputWeightProduct ns gs = sum $ zipWith (*) ns gs
