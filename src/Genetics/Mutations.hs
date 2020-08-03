module Genetics.Mutations where

import           Genetics.Defaults
import           Genetics.Type

import           Control.Concurrent (MVar, putMVar, readMVar, takeMVar)
import           Data.List          (find, sortBy)
import           Data.Map           (Map, toList, (!?))
import           Data.Map           (insert)
import           Data.Maybe         (catMaybes)
import           Data.Ord           (comparing)
import           System.Random      (randomIO, randomRIO)
import           Text.Pretty.Simple (pPrint)


mutateStruct ::
    MVar Int -> MVar Int -> MVar (Map (Int, Int) Int) -> Genotype -> IO Genotype
mutateStruct nodeMV innovMV connInnovMV g = do
    rnd <- randomRIO (0.0, 1.0)
    let pTot = smNodeAdditionProbability + smLinkAdditionProbability
    if rnd < smNodeAdditionProbability / pTot
        then mutateAddNode nodeMV innovMV connInnovMV g
        else mutateAddConnection nodeMV innovMV connInnovMV g

mutateAddNode ::
    MVar Int -> MVar Int -> MVar (Map (Int, Int) Int) -> Genotype -> IO Genotype
mutateAddNode nodeMV innovMV connInnovMV g = do
    let ns = nodes g
        cons = geneConnections g
        biasNodes = filter (\n -> nodeType n == Bias) ns
        nonBiasCons = filter
            (\c -> not (geneIn c `elem` map nodeNum biasNodes))
            cons
    rndIdx <- randomRIO (0, length nonBiasCons - 1)
    ciMap  <- readMVar connInnovMV
    let rndCon = nonBiasCons !! rndIdx
        a      = geneIn  rndCon
        b      = geneOut rndCon
        wAB    = geneWeight rndCon
    let knownDivisions = findKnownDivisions a b ciMap
        prevDivisions = findDivisions a b cons
    (newNode, axCon, xbCon) <- mutate' knownDivisions prevDivisions a b wAB
    let ns' = sortBy (comparing nodeNum) (newNode:ns)
        cons' = sortBy (comparing geneInnov) (xbCon:axCon:cons)
    let g' = g { nodes = ns', geneConnections = map (disableAB a b) cons' }
    return g'
  where
    mutate' knownDivisions prevDivisions a b wAB = case reverse knownDivisions of
        [] -> innovateNewConDivision nodeMV innovMV connInnovMV a b wAB
        lstKnownDiv : _ ->
            case reverse prevDivisions of
                [] -> return $ pickNextKnownMutation a b wAB 0 knownDivisions
                lstPrevDiv:_ -> do
                    let lstPrevInnov = geneInnov (snd lstPrevDiv)
                    if lstPrevInnov == snd (snd lstKnownDiv)
                        then innovateNewConDivision
                                nodeMV innovMV connInnovMV a b wAB
                        else return $ pickNextKnownMutation
                                a b wAB lstPrevInnov knownDivisions

    pickNextKnownMutation a b w lastDiv known =
        let (((_, x), innov), _) = pickNext lastDiv known
            n' = Node x Hidden 0.0
            c1' = Gene a x 1.0 True innov
            c2' = Gene x b w True (innov + 1)
        in (n', c1', c2')

    pickNext _ [] = error "Assumed to be non empty"
    pickNext innov (y@(((_, _), n), _):ns) =
        if innov < n
            then y
            else pickNext innov ns

    disableAB a b c = if geneIn c == a && geneOut c == b
        then c { geneEnabled = False }
        else c

innovateNewConDivision :: MVar Int
                       -> MVar Int
                       -> MVar (Map (Int, Int) Int)
                       -> Int
                       -> Int
                       -> Double
                       -> IO (Node, Gene, Gene)
innovateNewConDivision nextNodeMV nextInnovMV conInnovMV a b wAB = do
    nextNodeId <- takeMVar nextNodeMV
    putMVar nextNodeMV (nextNodeId + 1)
    let n' = Node
                { nodeNum = nextNodeId
                , nodeType = Hidden
                , activation = 0.0 }
    nextInnovId <- takeMVar nextInnovMV
    putMVar nextInnovMV (nextInnovId + 2)
    let c1' = Gene
                { geneIn = a
                , geneOut = nextNodeId
                , geneWeight = 1.0
                , geneEnabled = True
                , geneInnov = nextInnovId }
        c2' = Gene
                { geneIn = nextNodeId
                , geneOut = b
                , geneWeight = wAB
                , geneEnabled = True
                , geneInnov = nextInnovId + 1 }
    ciMap <- takeMVar conInnovMV
    let ciMap' = insert
                    (nextNodeId, b)
                    (nextInnovId + 1) $
                        (insert (a, nextNodeId) nextInnovId ciMap)
    putMVar conInnovMV ciMap'
    return (n', c1', c2')


findKnownDivisions ::
    Int -> Int -> Map (Int, Int) Int -> [(((Int, Int), Int), ((Int, Int), Int))]
findKnownDivisions a b cMap = catMaybes $ map justPair mayAXBPairs
  where
    cs = toList cMap

    aXInnovs = filter (\((x, _), _) -> x == a) cs

    mayAXBPairs = map
            (\ax@((_, x), aXInnov) ->
                ( ax
                , find
                    (\((yIn, yOut), innovY) ->
                            yIn == x
                            && yOut == b
                            && innovY == aXInnov + 1)
                    cs))
            aXInnovs

    justPair (_, Nothing) = Nothing
    justPair (x, Just y)  = Just (x, y)


findDivisions :: Int -> Int -> [Gene] -> [(Gene, Gene)]
findDivisions a b cs = catMaybes $ map justDivision mayABDivs
  where
    aCons  = filter (\c -> geneIn c == a) cs
    mayABDivs = map (\ac -> (ac, find (isConDivision ac) cs)) aCons

    justDivision (_, Nothing)      = Nothing
    justDivision (con1, Just con2) = Just (con1, con2)

    -- @x@ is first connection, @y@ is target connection
    isConDivision y x =
           geneInnov y + 1 == geneInnov x
        && geneIn  y == geneOut x
        && geneOut y == b


mutateAddConnection ::
    MVar Int -> MVar Int -> MVar (Map (Int, Int) Int) -> Genotype -> IO Genotype
mutateAddConnection nodeMV innovMV connInnovMV g = do
    -- First select output for new connection
    -- There is no sense to make connections to Sensors or Bias nodes
    let ns = nodes g
        nonBSNodes = filter isNotBiasOrSensor ns
    let cons = geneConnections g
    let nonBSCount = length nonBSNodes
    -- If all nodes are Biases or Sensors mutation have no sense
    if nonBSCount == 0
        then return g
        else do
            w      <- randomRIO (-1.0, 1.0)
            let possibleOutputs = unconnectedNodes g
            outIdx <- randomRIO (0, length possibleOutputs - 1)
            let oNode = possibleOutputs !! outIdx
                o     = nodeNum oNode
            -- Now we need to create NEW connection, so we have to
            -- filter candidates for new connection input
            let existingTriggers = filter (isConnTrigger oNode) cons
            let existing = map geneIn existingTriggers
            let ns' = filter (\n -> not (nodeNum n `elem` existing)) ns
            inIdx  <- randomRIO (0, length ns' - 1)
            let iNode = ns' !! inIdx
                i     = nodeNum iNode
            ciMap <- takeMVar connInnovMV
            c <- case ciMap !? (i, o) of
                Nothing    -> do
                    nextInnovId <- takeMVar innovMV
                    putMVar innovMV (nextInnovId + 1)
                    putMVar connInnovMV (insert (i,o) nextInnovId ciMap)
                    return $ Gene i o w True nextInnovId
                Just innov -> do
                    putMVar connInnovMV ciMap
                    return $ Gene i o w True innov
            let cons' = sortBy (comparing geneInnov) (c:cons)
            return $ g { geneConnections = cons'  }
  where
    isNotBiasOrSensor (Node _ Bias _)   = False
    isNotBiasOrSensor (Node _ Sensor _) = False
    isNotBiasOrSensor _                 = True


mutateWeights' :: Genotype -> IO Genotype
mutateWeights' g = do
    cons <- mapM mut (geneConnections g)
    return $ g { geneConnections = cons }
  where
    mut c = do
        mutateConn <- randomIO
        if not mutateConn || isBiasCon c
        then return c
        else do
            perturb <- randomRIO (0.0, 1.0)
            if perturb > uniformPerturbationWeightMutationProbability
                then do
                    rand <- randomRIO (0.0, 1.0)
                    if rand < randomWeightMutationProbability
                        then do
                            w' <- randomRIO (-1.0, 1.0)
                            return $ c { geneWeight = w' }
                        else return c
                else do
                    dw <- randomRIO (-learningRate, learningRate)
                    let w = geneWeight c
                    let w' = if w > 0.0
                                then min 1.0 (w + dw)
                                else max (-1.0) (w + dw)
                    return $ c { geneWeight = w' }

    biases = filter (\(Node _ t _)-> t == Bias) (nodes g)

    isBiasCon c = geneIn c `elem` (map nodeNum biases)


unconnectedNodes :: Genotype -> [Node]
unconnectedNodes g = fst $ unzip $ filter (\(_, cn) -> cn < lNodes) ncMap
  where
    lNodes = length (nodes g)

    nonBSNodes = filter nonBS (nodes g)

    ncMap = map cons' nonBSNodes

    cons' n =
        let cs = filter (\c -> geneOut c == nodeNum n) (geneConnections g)
        in (n, length cs)


    nonBS (Node _ Bias _)   = False
    nonBS (Node _ Sensor _) = False
    nonBS _                 = True
