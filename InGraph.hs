{-
	InGraph - Ingress link optimizer

    Copyright (C) 2013  Nigel D. Stepp

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

    Nigel Stepp <stepp@atistar.net>
	http://www.atistar.net/~stepp/ingraph/

    $Id: InGraph.hs 861 2014-08-18 17:34:13Z stepp $
-}

-- |Ingress Graph module.
-- The routines in this module support optimizing links between
-- portals in the game Ingress.
module InGraph where

import Data.List
import qualified Data.Set as Set
import Data.Maybe
import System.Random
import Data.Graph.Inductive
import GHC.Float

import Control.Monad.Random.Strict

-- * Types

data OptMode = LinkMode | FieldMode
newtype Vector a = Vec (a,a) deriving (Eq)
type Portal = (String,Vector Float)
type PortalGraph = Gr Portal ()
type Hamiltonian = Float -> PortalGraph -> Float

instance Show a => Show (Vector a) where
    show (Vec (x,y)) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Functor Vector where
    fmap f (Vec (x,y)) = Vec (f x, f y)

-- | Allow math to be used with a 'Vector'
instance Num a => Num (Vector a) where
    (+) (Vec (a1,a2)) (Vec (b1,b2)) = Vec (a1+b1, a2+b2)
    (-) (Vec (a1,a2)) (Vec (b1,b2)) = Vec (a1-b1, a2-b2)
    (*) (Vec (a1,a2)) (Vec (b1,b2)) = Vec (a1*b1, a2*b2)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger x = Vec (fromInteger x, fromInteger x)

-- * Functions

-- ** Graph handling routines

emptyGraph = empty :: PortalGraph

-- |Add a list of portals to a portal graph
addPortals :: PortalGraph -> [Portal] -> PortalGraph
addPortals = foldl (\ g p -> insNode (newNode g, p) g)

-- |Determine the id of the next node to add
newNode :: PortalGraph -> Node
newNode g = head (newNodes 1 g)

-- |Remove all of the edges from a graph
stripEdges :: PortalGraph -> PortalGraph
stripEdges g =
    delEdges (edges g) g

-- ** Mathematical Utilities

-- |Arithmetic mean
mean :: (Real a, Fractional b) => [a] -> b
mean x = realToFrac (sum x) / (genericLength x)

-- |Standard deviation
std :: (Real a, Fractional b) => [a] -> b
std x = realToFrac (foldl (\b c -> b + c^2) 0 (map (\a -> (realToFrac a) - (mean x)) x)) / (genericLength x - 1)

-- |Vector dot product
dot :: Num a => Vector a -> Vector a -> a
dot (Vec (x1,y1)) (Vec (x2,y2)) = x1*x2 + y1*y2

-- |Vector length
norm :: Floating a => Vector a -> a
norm v = sqrt (dot v v)

-- |Two dimensional cross product, used for link crossing
cross2D :: Num a => Vector a -> Vector a -> a
cross2D (Vec (v1,v2)) (Vec (w1,w2)) = v1*w2 - v2*w1

-- |Vector angle
vecAngle :: Floating a => Vector a -> Vector a -> a
vecAngle v w = acos $ (dot v w) / (norm v * norm w)

-- ** Graph queries

-- |Determine whether or not links between two pairs of portals
-- cross
linksCross :: (Portal,Portal) -> (Portal,Portal) -> Bool
linksCross link1 link2 =
    let p1 = snd (fst link1)
        p2 = snd (snd link1)
        p3 = snd (fst link2)
        p4 = snd (snd link2)
        v = p2 - p1
        w = p4 - p3
        denom = cross2D v w
    in if denom /= 0
       then let crossPointv = (cross2D (p3 - p1) w) / denom
                crossPointw = (cross2D (p3 - p1) v) / denom
            in (crossPointw > 0.0 && crossPointw < 1.0) && (crossPointv > 0.0 && crossPointv < 1.0)
       else False

-- |Count the number of fields adjacent to a portal.
-- Returns a list of endpoint pairs for each triangle.
-- For instance:
--
-- >>> adjFields g 1
-- [(2,3),(3,4)]
-- 
-- Specifies that node 1 makes triangles (1,2,3) and (1,3,4).
adjFields :: PortalGraph   -- ^ Graph to analyze
          -> Node          -- ^ Find fields with this node as a vertex
          -> [(Node,Node)] -- ^ A list containing pairs of vertices that
                           -- make a trianlge with the given reference vertex
adjFields g p = let links = suc (undir g) p
                    endpoints = map (intersect links . suc g) links
                    endpairs = map (\a -> zip (repeat (fst a)) (snd a) ) (zip links endpoints)
                in concat endpairs

-- |Return a collection of fields present in a graph.
-- Fields are returned as a list of node lists. Each
-- node list contains the 3 nodes that make up a particular
-- field.
getFields :: PortalGraph -> [[Node]]
getFields g =
    let allAdjFields = map (adjFields g) (nodes g)
        fieldVerts = zipWith (\a b -> map (\c-> [a, fst c, snd c]) b)  (nodes g) allAdjFields
        uniqueFields = nub $ map Set.fromList $ concat fieldVerts
    in map Set.toList uniqueFields

-- |Extract the position of a portal
portalVec :: PortalGraph -> Node -> Vector Float
portalVec g n = snd $ fromJust $ lab g n

-- |Determine whether a portal is covered by a particular field
isCoveredBy :: PortalGraph
            -> Node        -- ^ The portal node id
            -> [Node]      -- ^ Three element list of field vertices
            -> Bool
isCoveredBy g portal field =
    let p = portalVec g portal
        fieldVecs = map (\x -> portalVec g x - p) field
        vecAngles = zipWith vecAngle fieldVecs (tail $ cycle fieldVecs)
    in abs ((sum vecAngles) - (2*pi)) < 1e-6

-- |Determine whether a portal is covered by any field
isCovered :: PortalGraph
          -> Node -- ^ The portal node id
          -> Bool
isCovered g portal =
    let fields = getFields g
    in any (isCoveredBy g portal) fields


-- |Count how many fields are covering a portal
countCoverings :: PortalGraph
               -> Node        -- ^ The portal node id
               -> Int         -- ^ Number of covering fields
countCoverings g portal =
    let fields = getFields g
    in length $ filter (isCoveredBy g portal) fields


-- |Calculate the area covered by a field
fieldArea :: Portal -> Portal -> Portal -> Float
fieldArea portal1 portal2 portal3 =
    let p1 = snd portal1
        p2 = snd portal2
        p3 = snd portal3
        a = norm (p2-p1)
        b = norm (p3-p2)
        c = norm (p1-p3)
        s = (a+b+c)/2
    in sqrt (s*(s-a)*(s-b)*(s-c))
    where norm (Vec (x,y)) = sqrt (x^2+y^2)

-- |Calculate the area covered by a field,
-- using graph node numbers
fieldAreaG :: PortalGraph -> Node -> Node -> Node -> Float
fieldAreaG g p1 p2 p3 =
    let portal1 = fromJust $ lab g p1
        portal2 = fromJust $ lab g p2
        portal3 = fromJust $ lab g p3
    in fieldArea portal1 portal2 portal3

-- |Determine whether a potential link would cross
-- an existing link
makesCross :: PortalGraph
           -> (Node,Node) -- ^ Tuple containing from and to portal ids
           -> Bool
makesCross g (from,to) =
    let fromNodeM = lab g from
        toNodeM = lab g to
    in case fromNodeM of
        Nothing -> error $ "Missing from node " ++ show from ++ " out of " ++ show (labNodes g)
        Just fromNode -> case toNodeM of
            Nothing -> error $ "Missing to node " ++ show to ++ " out of " ++ show (labNodes g)
            Just toNode -> or $ crosses fromNode toNode
    where
        crosses fromNode toNode = map (\(a,b)->linksCross (fromNode,toNode) (fromJust $ lab g a, fromJust $ lab g b)) (edges g)


-- ** Optimization

-- |#hamiltonians#

-- *** Hamiltonian Functions

-- |Hamiltonian for the \"Save Flynn\" event
hamiltonianFlynn :: Float -> PortalGraph -> Float
hamiltonianFlynn gain g = -(fromIntegral $ flynnScore g)
    where
        flynnScore :: PortalGraph -> Int
        flynnScore g =
            let portals = noNodes g
                links = sum $ map (deg g) (nodes g)
                fields = countFields g
            in portals + 5*links + 10*fields

-- |Hamiltonian for maximizing the number of fields.
-- This function has a hard time when optimizing in link mode.
-- For best results, use in field mode.
hamiltonianFields :: Float -> PortalGraph -> Float
hamiltonianFields gain g =
    let links = countLinks g
        fields = countFields g
    in -(fromIntegral $ links + 100*fields)

-- |Hamiltonian for maximizing global number of links.
hamiltonianLinks :: Float -> PortalGraph -> Float
hamiltonianLinks gain g =
    let links = countLinks g
    in -(fromIntegral links)


hamiltonianKeys :: Float -> PortalGraph -> Float
hamiltonianKeys gain g =
    let avgIn = mean $ map (indeg g) (nodes g)
        degree = map (deg g) (nodes g)
        deg1 = length $ filter (<2) degree
        deg0 = length $ filter (<1) degree
    in -(fromIntegral (500*deg1 + 1000*deg0) + avgIn)

hamiltonianMaxCover :: Float -> PortalGraph -> Float
hamiltonianMaxCover gain g =
    let coverings = countCoverings g 1
    in -(fromIntegral coverings)

-- |Hamiltonian for maximizing degree, while keeping the degree
-- as constant as possible.
hamiltonianDegree :: Float -> PortalGraph -> Float
hamiltonianDegree gain g =
    let links = map (deg g) (nodes g)
        degree = mean links
        degreeStd = std links
    in (1.0 + degreeStd) / degree

-- |Hamiltonian for maximizing portal defense due to links.
-- There is a defensive boost to shields according to
-- 
-- > Boost = 4/9 * atan(L/exp(1))
-- 
-- where L is the number of links.
hamiltonianLinkDefense :: Float -> PortalGraph -> Float
--hamiltonianLinkDefense gain g = sum $ map (\n-> exp (8.0-(fromIntegral $ deg g n))) (nodes g)
hamiltonianLinkDefense gain g = -linkDefense
    where
        linkDefense = sum $ map (\n -> 4.0/9.0 * atan (fromIntegral (deg g n)/(exp 1))) (nodes g)

-- |Hamiltonian for maximizing AP due to creating links and fields
hamiltonianAP :: Float -> PortalGraph -> Float
hamiltonianAP gain g =
    let links = countLinks g
        fields = countFields g
    in -(fromIntegral $ 313 * links + 1250 * fields)

-- |Hamiltonian for minimizing the ratio of AP to destroy to AP to create.
hamiltonianRatio :: Float -> PortalGraph -> Float
hamiltonianRatio gain g = let energies = map (portalEnergy gain g) (nodes g)
                in sum energies

-- |Hamiltonian for maximizing the score at the UCLA Warp Break event
hamiltonianUCLA :: Float -> PortalGraph -> Float
hamiltonianUCLA gain g =
    let
        portals = noNodes g
        coverings = countCoverings g 1
        score = portals + 4 + (coverings * 25)
    in if coverings < 1
       then 1e6
       else -(fromIntegral score)

-- |Calculate the energy of a single portal.
-- Energy is defined to be the ratio of AP
-- due to destruction over AP due to creation.
-- This is then scaled by the number of fields.
portalEnergy :: Float       -- ^ Importance of making fields, 0 to 1
             -> PortalGraph -- ^ The current graph
             -> Node        -- ^ Calculate the energy of this node
             -> Float       -- ^ Returns portal energy
portalEnergy fieldGain g p =
    let links = deg g p
        fields = adjFields g p
        numFields = length fields
        --area = foldl (\a f -> a + (fieldAreaG g p (fst f) (snd f))) 0 fields
        enemyAP = fromIntegral (187*links + 750*numFields)
        friendlyAP = fromIntegral (313*links + 1250*numFields)
    in if links < 1
          then 1e6
          else enemyAP / friendlyAP / (1 - fieldGain + fieldGain * tanh (fromIntegral (1+numFields)))

-- *** Optimizers

-- |Implement a simple metropolis algorithm for minimizing
-- the hamiltonian of a portal graph
metropolisWith :: MonadRandom m =>
            Hamiltonian -- ^ Hamliltonian function
            -> PortalGraph -- ^ Graph to optimize
            -> Float       -- ^ Importance of making fields, 0 to 1
            -> Int         -- ^ Maximum number of iterations
            -> m PortalGraph -- ^ Returns the optimized graph
metropolisWith h g gain 0 = do
    return g
metropolisWith h g gain n = do
    temp <- perturbGraph g
    if (h gain temp) < (h gain g)
        then metropolisWith h temp gain (n-1)
        else metropolisWith h g gain (n-1)
--    where
--        iteraten f x 0 = x
--        iteraten f x n = iteraten f (f x) (n-1)

-- |Implement a simple metropolis algorithm for minimizing
-- the hamiltonian of a portal graph
fieldMetropolisWith :: MonadRandom m =>
            Hamiltonian -- ^ Hamliltonian function
            -> PortalGraph -- ^ Graph to optimize
            -> Float       -- ^ Importance of making fields, 0 to 1
            -> Int         -- ^ Maximum number of iterations
            -> [[Node]]     -- ^ List of fields
            -> m PortalGraph -- ^ Returns the optimized graph
fieldMetropolisWith h g gain 0 fields = do
    return $ graphFromFields g fields
fieldMetropolisWith h g gain n fields =
    do
        let minimalGraph = graphFromFields g fields
        (temp,newFields) <- perturbGraphFields (minimalGraph,fields)
        let newGraph = if (h gain temp) < (h gain minimalGraph)
                        then temp
                        else minimalGraph
        fieldMetropolisWith h newGraph gain (n-1) newFields

-- |Generate a random field
randomField :: MonadRandom m => PortalGraph -> m [Node]
randomField g =
    pickPortals g []
    where
        pickPortals :: MonadRandom m => PortalGraph -> [Node] -> m [Node]
        pickPortals g portals =
            if length portals >= 3
            then return portals
            else do newPortal <- getRandomR (0, (noNodes g)-1)
                    pickPortals g (nub (newPortal : portals))

-- |Create a random modification to a graph, at the level of fielids
-- Pick 3 random nodes
-- If legal, add it to a list of fields in the graph.
-- If illegal, remove a field and repeat
perturbGraphFields :: MonadRandom m => (PortalGraph, [[Node]]) -> m (PortalGraph, [[Node]])
perturbGraphFields (g,fields) = do
    newField <- randomField g
    let fieldLinks = [ (x,y) | x <- newField, y <- newField, x < y ]

    if not (null fields) && any (makesCross g) fieldLinks
        then do f <- getRandomR (0, (length fields)-1)
                let newFields = delete (fields!!f) fields
                perturbGraphFields (graphFromFields g newFields, newFields)
        else let newFields = newField : fields
             in return (graphFromFields g newFields, newFields)

graphFromFields :: PortalGraph -> [[Node]] -> PortalGraph
graphFromFields g fields =
    addFields (stripEdges g) fields
    where
        addFields g [] = g
        addFields g (f:fs) =
            let fieldEdges = filter (\e -> e `notElem` labEdges g)
                                    [ (x,y,()) | x <- f, y <- f, x < y ]
            in addFields (insEdges fieldEdges g) fs


-- |Create a random modification to a graph.
-- Pick a link to toggle on or off.
-- Continue toggling links until one of the toggles adds a link.
-- This is because the hamiltonian will nearly always judge removal of a link as a higher energy state.
perturbGraph :: MonadRandom m => PortalGraph -> m PortalGraph
perturbGraph g = do
    -- Select two random nodes for a link
    randLinkFrom <- getRandomR (0, (noNodes g)-1)
    randLinkTo <- getRandomR (0, (noNodes g)-1)
    --- If it's a self-link, try again
    if randLinkFrom == randLinkTo
       then if length (nodes g) < 2
            then return g
            else perturbGraph g
       else
          --Toggle links until a toggle adds a link
          if (randLinkFrom,randLinkTo) `elem` edges g
             then perturbGraph (delEdge (randLinkFrom,randLinkTo) g)
             else if (randLinkTo,randLinkFrom) `elem` edges g
                  then perturbGraph (delEdge (randLinkTo,randLinkFrom) g)
                  else if makesCross g (randLinkFrom,randLinkTo)
                       then perturbGraph g
                       else return $ insEdge (randLinkFrom,randLinkTo,()) g

-- |Optimize the links among a collection of nodes.
graphOptimizeWith :: MonadRandom m =>
              Hamiltonian -- ^ Hamiltonian function
              -> Float       -- ^ fieldGain: Importance of making fields, from 0 to 1
              -> Int         -- ^ maxIter: Maximum optimzation iterations
              -> OptMode     -- ^ optMode: Optimization mode
              -> PortalGraph -- ^ portalGraph: Graph to optimize
              -> m PortalGraph -- ^ Returns optimized graph
graphOptimizeWith h gain maxiter optMode portalGraph =
    case optMode of
        LinkMode -> metropolisWith h portalGraph gain maxiter
        FieldMode -> fieldMetropolisWith h portalGraph gain maxiter []


-- ** Graph measures

-- |Count the total number of links in a graph
countLinks :: PortalGraph -> Int
countLinks g = sum $ map (indeg g) (nodes g)

-- |Count the total number of fields created by links in a graph
countFields :: PortalGraph -> Int
countFields g = length (getFields g)

-- |Rank portals by importance. See "rankPortalNodes".
rankPortals :: PortalGraph -> [Portal]
rankPortals g = map (fromJust . lab g) (rankPortalNodes g)

-- |Rank portal nodes according to how important they are.
-- Importance is ranked by AP due to destruction.
rankPortalNodes :: PortalGraph -> [Node]
rankPortalNodes g =
    let links = map (deg g) (nodes g)
        fields = map (length . adjFields g) (nodes g)
        aps = zipWith (\x y -> 187*x + 750*y) links fields
        apsIdx = zip aps (nodes g)
    in map snd $ reverse $ sort apsIdx



-- |Calculate some descriptive numbers summarizing a graph.
-- Return a tuple of (Enemy AP, Friendly AP, Number of Fields)
graphStats :: PortalGraph          -- ^ The graph to analyze
           -> (Int, Int, Int, Int) -- ^ Returns enemy AP, friendly AP,
                                   --   the number of fields, and links  as a tuple
graphStats g =
    let links = sum $ map (outdeg g) (nodes g)
        numFields = countFields g
        enemyAP = 187*links + 750*numFields
        friendlyAP = 313*links + 1250*numFields
    in (enemyAP,friendlyAP,numFields,links)


-- * Misc


-- Sample portal list, used for testing
portals = [
--    ("Montana Ave Library", Vec (17.1,10.0)),
--    ("Douglas park", Vec (25.0,6.1)),
--    ("Mural (new balance)", Vec (26.1,6.0)),
--    ("Artistic house", Vec (26.0,7.0)),
--    ("St. Monica (church)", Vec (7.1,7.1)),
--    ("Post Office (26th St)", Vec (26.0,14.1)),
--    ("Museum Systems", Vec (27.0,14.0))
    ("Montana Ave Library", Vec (430,232)),
    ("Douglas park", Vec (588,221)),
    ("Mural (new balance)", Vec (605,213)),
    ("Artistic house", Vec (581,191)),
    ("St. Monica (church)", Vec (372,395)),
    ("Post Office (26th St)", Vec (447,51)),
    ("Museum Systems", Vec (461,41)),
    ("Palisades Branch Library", Vec (46,102)),
    ("Fire Station 69", Vec (77,86)),
    ("Post Office (Palisades)", Vec (40,72))
    ]

-- |Sample portal graph, for testing
northSaMo = addPortals emptyGraph portals


