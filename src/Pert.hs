{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards
           , OverloadedStrings, TupleSections #-}

module Pert
  ( Graph(..), Activity(..), Node(..), Edge(..)
  , ActivityId(..), NodeId(..), EdgeId(..)
  , ActivityMap, NodeMap, EdgeMap
  , graph
  ) where

import GHC.Generics

import Control.Applicative

import Data.Functor.Contravariant

import Data.Char
import Data.Maybe
import Data.List
import Data.Aeson

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


data Graph = Graph
  { grActivities :: ActivityMap
  , grNodes :: NodeMap
  , grEdges :: EdgeMap
  } deriving Generic

type ActivityMap = Map ActivityId Activity
type NodeMap = Map NodeId Node
type EdgeMap = Map EdgeId Edge

type DepMap = Map ActivityId NodeId
type DestMap = Map NodeId (Map NodeId [ActivityId])
type PrevMap = DestMap
type NextMap = DestMap

newtype ActivityId = ActivityId { getActivityId :: Text }
  deriving ( Show, Generic, Eq, Ord)
newtype NodeId = NodeId { getNodeId :: Set ActivityId }
  deriving ( Show, Generic, Eq, Ord)
newtype EdgeId = EdgeId { getEdgeId :: (NodeId, NodeId) }
  deriving ( Show, Generic, Eq, Ord)

instance FromJSONKey ActivityId where
  fromJSONKey = fmap ActivityId fromJSONKey 
instance ToJSONKey ActivityId where
  toJSONKey = contramap getActivityId toJSONKey
instance FromJSON ActivityId where
  parseJSON = fmap ActivityId . parseJSON
instance ToJSON ActivityId where
  toJSON     = toJSON . getActivityId
  toEncoding = toEncoding . getActivityId


data Activity =
  Activity
  { actDescr :: Text
  , actDuration :: Int
  , actDependsOn :: Set ActivityId
  } deriving Generic

jsonOptions = defaultOptions
    { fieldLabelModifier = dropFirst }

dropFirst :: String -> String
dropFirst = lowerFirst . dropWhile isLower
  where lowerFirst (c:cs) = toLower c : cs
        lowerFirst [] = []

instance ToJSON Activity where
  toJSON     = genericToJSON jsonOptions
  toEncoding = genericToEncoding jsonOptions
instance FromJSON Activity where
  parseJSON     = genericParseJSON jsonOptions

data Node =
  Node
  { nodePrev :: [EdgeId]
  , nodeNext :: [EdgeId]
  , nodeName :: Int
  , nodeEarliest :: Int
  , nodeLatest :: Int
  }

data Edge =
  Edge
  { edgeActivities :: [ActivityId]
  , edgeFrom :: NodeId
  , edgeTo :: NodeId
  , edgeEarliestStart :: Int
  , edgeEarliestFinish :: Int
  , edgeLatestStart :: Int
  , edgeLatestFinish :: Int
  }


graph :: ActivityMap -> Graph
graph as = Graph{..}
  where deps = getDeps as
        nodes = getNodes as deps
        acts = actEdges deps nodes
        dests = destMap acts
        prevs = prevMap dests nodes
        grNodes = nodeMap nodes prevs as
        grEdges = edgeMap grNodes as prevs
        grActivities = as

{- Operations on NodeIds -}

intersection :: NodeId -> NodeId -> NodeId
intersection (NodeId a) (NodeId b) = NodeId $ a `S.intersection` b

isSubsetOf :: NodeId -> NodeId -> Bool
NodeId a `isSubsetOf` NodeId b = a `S.isSubsetOf` b

isProperSubsetOf :: NodeId -> NodeId -> Bool
NodeId a `isProperSubsetOf` NodeId b = a `S.isProperSubsetOf` b

after :: [NodeId] -> NodeId -> Bool
after ns n = any (`isProperSubsetOf` n) ns

before :: [NodeId] -> NodeId -> Bool
before ns n = any (n `isProperSubsetOf`) ns


{-|
Map each activity to the set of all its
direct and indirect dependencies.
-}
getDeps :: ActivityMap -> DepMap
getDeps as = fmap NodeId depmap
  where depmap = fmap deps as
        deps Activity{..} = foldr S.union actDependsOn
          $ S.map (depmap M.!) actDependsOn

{-|
Find the set of all nodes required in the graph.
-}
getNodes :: ActivityMap -> DepMap -> Set NodeId
getNodes as ds = addIntersections $ S.insert finish deps
  where deps = S.fromList $ M.elems ds
        finish = NodeId $ M.keysSet as

{-|
Add separate nodes for intersections of node dependencies,
required for dummies.
-}
addIntersections :: Set NodeId -> Set NodeId
addIntersections ns = S.fromList $ concatMap intersections nodeTails
  where intersections (n,ns') = map (intersection n) ns'
        nodeTails = zip nodes $ tails nodes
        nodes = S.toList ns
{-|
Map activities to edges.
-}
actEdges :: DepMap -> Set NodeId -> Map ActivityId EdgeId
actEdges ds ns = M.mapWithKey (actEdge ns) ds

{-|
Find the edge for an activity. An activity starts at the node
that exactly fulfills its dependencies and ends at the earliest
node containing at least its dependencies and itself. 
-}
actEdge :: Set NodeId -> ActivityId -> NodeId -> EdgeId
actEdge ns a from = EdgeId (from, to)
  where done = NodeId $ S.insert a $ getNodeId from
        tos = filter (done `isSubsetOf`) $ S.toList ns
        to = head $ filter (not . after tos) tos

{-|
Find the map of previous nodes and activitites for each
destination node.
-}
destMap :: Map ActivityId EdgeId -> DestMap
destMap es = foldr union M.empty $ map single $ M.toList es
  where single (a, EdgeId (f,t)) = M.singleton t $ M.singleton f [a]
        union = M.unionWith (M.unionWith (<>))

{-|
Map each node to its incoming edges.
-}
prevMap :: DestMap -> Set NodeId -> PrevMap
prevMap dests ns = M.fromSet (prev dests ns) ns

{-|
Find incoming edges for a node, including dummies.
-}
prev :: DestMap -> Set NodeId -> NodeId -> Map NodeId [ActivityId]
prev dests ns n = acts `M.union` dummies
  where acts = fromMaybe M.empty $ M.lookup n dests
        dummies = M.fromList $ map (,[])
          $ filter (not . before froms) froms
        froms = filter isMissing $ S.toList ns
        isMissing (NodeId n) = not (S.null n)
          && n `S.isSubsetOf` missing
        missing = getNodeId n `S.difference` found
        found = foldr S.union S.empty
          $ map (\(NodeId f, as) -> S.fromList as `S.union` f)
          $ M.toList acts
{-|
Flip around a PrevMap to get a NextMap.
-}
nextMap :: PrevMap -> NodeId -> NextMap
nextMap prevs finish = foldr add end $ concatMap flatten $ M.toList prevs
  where flatten (t,es) = map (\(f,as) -> (f,(t,as))) (M.toList es)
        add (f,(t,as)) = M.insertWith M.union f (M.singleton t as)
        end = M.singleton finish M.empty

{-|
Construct the final edge map.
-}
edgeMap :: NodeMap -> ActivityMap -> PrevMap -> EdgeMap
edgeMap nodes acts = M.fromList . map edge . concatMap flatten . M.toList
  where flatten (t,es) = map (t,) $ M.toList es
        edge (edgeTo,(edgeFrom,edgeActivities)) =
          (EdgeId (edgeFrom, edgeTo), Edge{..})
          where edgeEarliestStart = nodeEarliest (nodes M.! edgeFrom)
                edgeLatestFinish = nodeLatest (nodes M.! edgeTo)
                edgeEarliestFinish = edgeEarliestStart + maxDuration
                edgeLatestStart = edgeLatestFinish - maxDuration
                maxDuration = foldr max 0
                  $ map (actDuration . (acts M.!)) edgeActivities

{-|
Construct the final node map.
-}
nodeMap :: Set NodeId -> PrevMap -> ActivityMap -> NodeMap
nodeMap ns prevs acts = M.fromList $ map node $ S.toList ns
  where node n = (n, Node{..})
          where nodePrev = map (\f -> EdgeId (f,n))
                           $ M.keys $ prevs M.! n
                nodeNext = map (\t -> EdgeId (n,t))
                           $ M.keys $ nexts M.! n
                nodeName = names M.! n
                nodeLatest = latests M.! n
                nodeEarliest = earliests M.! n
        finish = NodeId $ M.keysSet acts
        nexts = nextMap prevs finish
        expected = earliest finish
        names = M.fromList $ zip (sortOn earliest $ S.toList ns) [1..]
        earliests = M.fromSet earliest ns
        latests = M.fromSet latest ns
        earliest n = foldr max 0
          $ map (\(n,as) -> earliests M.! n + maxDuration as)
          $ M.toList (prevs M.! n)
        latest n = foldr min expected
          $ map (\(n,as) -> latests M.! n - maxDuration as)
          $ M.toList (nexts M.! n)
        maxDuration = foldr max 0 . map (actDuration . (acts M.!))
