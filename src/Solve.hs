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
solutionTimeout = 5 * 1000 * 1000

populationCoefficient :: Int
populationCoefficient = 4

populationSize :: Integer
populationSize = fromIntegral populationCoefficient * 300


-- * Solution

runNiche :: TChan Value -> Species -> IO [Int]
runNiche ch = mapM (runXORSpecies ch) . individuals

runXORSpecies :: TChan Value -> IndividualAI -> IO Int
runXORSpecies ch ai = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    m3 <- newEmptyMVar
    m4 <- newEmptyMVar
    _ <- concurrently
        (concurrently
            (runSpeciesInput m1 ai (False, False))
            (runSpeciesInput m2 ai (True, False)))
        (concurrently
            (runSpeciesInput m3 ai (False, True))
            (runSpeciesInput m4 ai (True, True)))
    r1' <- readMVar m1
    r2' <- readMVar m2
    r3' <- readMVar m3
    r4' <- readMVar m4
    let r1 = head' r1'
        r2 = head' r2'
        r3 = head' r3'
        r4 = head' r4'
    let results = [r1, r2, r3, r4]
    let expect = [False, True, True, False]
    let f   = (4 - (sum . fitness $ zip results expect))^(2 :: Int)
    atomically $ writeTChan ch (object [ "results" .= toJSON [r1', r2', r3', r4']
                                       , "fitness" .= f
                                       , "output"  .= toJSON [ xorAIOutputIsActive (snd r1)
                                                             , xorAIOutputIsActive (snd r2)
                                                             , xorAIOutputIsActive (snd r3)
                                                             , xorAIOutputIsActive (snd r4)
                                                             ] ] )
    return f
  where
    head' []    = error "Expected non empty state"
    head' (x:_) = x

    fitness rs = flip map rs $ \((done, g), e) ->
        if not done
        then 0
        else if (xorAIOutputIsActive g == e)
            then 1
            else 0

    xorAIOutputIsActive g = case filter (\(Node _ t _) -> t == Output) (nodes g) of
        ((Node _ Output a):_) -> a
        _                     -> False

runSpeciesInput ::
    MVar [(Bool, Genotype)] -> IndividualAI -> (Bool, Bool) -> IO ()
runSpeciesInput mv ai inp = do
    -- putStrLn $ "Running AI " ++ pack (show (aiId ai)) ++ " " ++ pack (show inp)
    let g = setAIInput inp (genome ai)
    putMVar mv [(False, g)]
    race_
        (threadDelay solutionTimeout)
        (feedForwardLoop mv)
    -- putStrLn $ "Done " ++ pack (show (aiId ai)) ++ " " ++ pack (show inp)

setAIInput :: (Bool, Bool) -> Genotype -> Genotype
setAIInput (i1, i2) g = g { nodes = nodes' }
  where
    nodes' = flip map (nodes g) $ \n ->
        case nodeNum n of
        1 -> n { activated = i1 }
        2 -> n { activated = i2 }
        _ -> n


feedForwardLoop :: MVar [(Bool, Genotype)] -> IO ()
feedForwardLoop mv = do
    state <- readMVar mv
    case state of
        [] -> error "Assuming non-empty state"
        ((done, s):states) -> do
            if done
            then return ()
            else do
                s' <- feedForward s
                _ <- swapMVar mv (s':states)
                return ()

feedForward :: Genotype -> IO (Bool, Genotype)
feedForward g = do
    let (respondents, _, _) = signalActivations g
    let g' = deactivateSANodes g respondents
    if haveUnspreadSignal g'
        then return (False, g')
        else return (True, g')


haveUnspreadSignal :: Genotype -> Bool
haveUnspreadSignal g =
    let active = genomeActivatedSANodes g
    in any (\c -> geneIn c `elem` map nodeNum active) (geneConnections g)


-- | Returns a list of node activations with list of active connections
--   depending on current signal spread in neuron nodes.  In other words
--   this function returns next state of nodes during signal spread or
--   this is a reaction of neurons for current signal state.
signalActivations :: Genotype -> ([(Node, Bool)], [Node], [Gene])
signalActivations g = (candidateActivations, activeSANodes, activeCons)
  where
    cons'                    = geneConnections g
    activeSANodes            = genomeActivatedSANodes g
    (biasNodeCons, restCons) = groupNodeCons isBiasConn (nodes g) cons' False
    (activeSANodeCons, _)    =
            groupNodeCons isInOfConnection activeSANodes restCons True
    (_, biasCons)            = unzip biasNodeCons
    (_, activeSACons)        = unzip activeSANodeCons
    activeCons               = concat biasCons ++ concat activeSACons
    (candidateNodeCons, _)   =
            groupNodeCons isOutOfConnection (nodes g) activeCons False
    react (n, gs)            = (n, (heavySideStep . inputWeightProduct) gs)
    candidateActivations     = map react candidateNodeCons

deactivateSANodes :: Genotype -> [(Node, Bool)] -> Genotype
deactivateSANodes g keep = g { nodes = nodes' }
  where
    nodes' = map update' (nodes g)
    update' n =
        let f = find (\(x, _) -> nodeNum x == nodeNum n) keep
        in case (f, n) of
            -- bias'es are always activated
            (_, (Node _ Bias _))         -> n {activated = True }
            -- keep output as is if new reaction found
            (Nothing, (Node _ Output _)) -> n
            -- deactivate sensors and hidden nodes if no reaction found
            (Nothing, _)                 -> n { activated = False }
            -- otherwise set node state corresponding to its reaction
            (Just (_, s), _)             -> n { activated = s }


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


genomeActivatedSANodes :: Genotype -> [Node]
genomeActivatedSANodes g = filter isActiveSensorAssocNode (nodes g)

isBiasConn :: Node -> Gene -> Bool
isBiasConn (Node n Bias _) c = geneIn c == n
isBiasConn _ _               = False

isActiveSensorAssocNode :: Node -> Bool
isActiveSensorAssocNode (Node _ Sensor True) = True
isActiveSensorAssocNode (Node _ Hidden True) = True
isActiveSensorAssocNode _                    = False

isInOfConnection ::  Node -> Gene -> Bool
isInOfConnection n с = geneIn с == nodeNum n

isOutOfConnection :: Node -> Gene -> Bool
isOutOfConnection n c = geneOut c == nodeNum n


heavySideStep :: Double -> Bool
heavySideStep = (>= 0)

inputWeightProduct :: [Gene] -> Double
inputWeightProduct = sum . map geneWeight

isTrigger :: Int -> Gene -> Bool
isTrigger nNum conn = geneOut conn == nNum

