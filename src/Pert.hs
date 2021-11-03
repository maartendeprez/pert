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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

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

type PrevMap = Map NodeId (Set (NodeId, Maybe ActivityId))


graph :: ActivityMap -> Graph
graph as = Graph{..}
  where deps = getDeps as
        nodes = depGroups as deps
        acts = actEdges deps nodes
        prevs = prevMap acts nodes
        grNodes = nodeMap nodes prevs as
        grEdges = edgeMap grNodes as prevs
        grActivities = as

{-|
Map each activity to the set of all its
direct and indirect dependencies.
-}
getDeps :: ActivityMap -> Map ActivityId NodeId
getDeps as = fmap (NodeId . deps) as
  where deps Activity{..} = foldr S.union actDependsOn
          $ S.map (\a -> deps (as M.! a)) actDependsOn

depGroups :: ActivityMap -> Map ActivityId NodeId -> Set NodeId
depGroups as ds = addIntersections $ S.insert finish deps
  where deps = S.fromList $ M.elems ds
        finish = NodeId $ M.keysSet as

addIntersections :: Set NodeId -> Set NodeId
addIntersections ds = S.fromList $ concatMap intersections $ S.toList ds
  where intersections d = map (intersection d) $ S.toList ds
        intersection (NodeId a) (NodeId b) = NodeId
          $ a `S.intersection` b

actEdges :: Map ActivityId NodeId -> Set NodeId -> Map ActivityId EdgeId
actEdges ds ns = M.mapWithKey edge ds
  where edge a from = EdgeId (from, to)
          where done = S.insert a $ getNodeId from
                tos = filter (\(NodeId n) -> done `S.isSubsetOf` n)
                  $ S.toList ns
                to = head $ filter (not . after tos) tos

isProperSubsetOf :: NodeId -> NodeId -> Bool
NodeId a `isProperSubsetOf` NodeId b = a `S.isProperSubsetOf` b

after :: [NodeId] -> NodeId -> Bool
after ns n = any (`isProperSubsetOf` n) ns

before :: [NodeId] -> NodeId -> Bool
before ns n = any (n `isProperSubsetOf`) ns

prevMap :: Map ActivityId EdgeId -> Set NodeId -> PrevMap
prevMap es ns = M.fromSet prev ns
  where dests = foldr dest M.empty $ M.toList es
        dest (a, EdgeId (f,t)) = M.insertWith S.union t
                                 (S.singleton (f, Just a))
        prev n = acts `S.union` S.fromList (map (,Nothing) dummies)
          where acts = fromMaybe S.empty $ M.lookup n dests
                dummies = filter (not . before froms) froms
                froms = filter isMissing $ S.toList ns
                isMissing (NodeId n) = not (S.null n)
                  && n `S.isSubsetOf` missing
                missing = getNodeId n `S.difference` found
                found = foldr S.union S.empty
                  $ map (\(NodeId f, Just a) -> S.insert a f)
                  $ S.toList acts

edgeMap :: NodeMap -> ActivityMap -> PrevMap -> EdgeMap
edgeMap nodes acts ps = M.fromList
  $ map (\(t,(f,a)) -> (EdgeId (f,t), Edge
                         { edgeFrom = f
                         , edgeTo = t
                         , edgeActivities = maybeToList a
                         , edgeEarliestStart = nodeEarliest (nodes M.! f)
                         , edgeEarliestFinish = nodeEarliest (nodes M.! f) + maybe 0 (\a -> actDuration (acts M.! a)) a
                         , edgeLatestStart = nodeLatest (nodes M.! t) - maybe 0 (\a -> actDuration (acts M.! a)) a
                         , edgeLatestFinish = nodeLatest (nodes M.! t)
                         }))
  $ concatMap (\(t,es) -> map (t,) $ S.toList es)
  $ M.toList ps

nodeMap :: Set NodeId -> PrevMap -> ActivityMap -> NodeMap
nodeMap ns prevs acts = M.fromList $ map makeNode $ S.toList ns
  where makeNode n = (n, Node
                         { nodePrev = []
                         , nodeNext = []
                         , nodeName = names M.! n
                         , nodeLatest = latest n
                         , nodeEarliest = earliest n
                         })
        finish = NodeId $ M.keysSet acts
        expected = earliest finish
        names = M.fromList $ zip (sortOn earliest $ S.toList ns) [1..]
        earliest n = foldr max 0
          $ map (\(n,a) -> earliest n + maybe 0 (\a -> actDuration (acts M.! a)) a)
          $ S.toList (prevs M.! n)
        latest n = foldr min expected
          $ map (\(n,a) -> latest n - maybe 0 (\a -> actDuration (acts M.! a)) a)
          $ S.toList (nexts M.! n)
        nexts = foldr (\(f,v) -> M.insertWith S.union f (S.singleton v)) (M.singleton finish S.empty)
          $ concatMap (\(t,ps) -> map (\(f,a) -> (f,(t,a))) (S.toList ps))
          $ M.toList prevs
