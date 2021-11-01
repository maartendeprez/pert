{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards
           , OverloadedStrings #-}

module Pert
  ( ActivityId(..), NodeId(..), EdgeId(..)
  , Activity(..), Node(..), Edge(..)
  , ActivityMap, NodeMap, EdgeMap
  , depGroups, depMap, nextMap, actEdges
  , prevMap, edgeMap, nameNodes, nodeMap
  ) where

import GHC.Generics

import Control.Applicative

import Data.Functor.Contravariant

import Data.Char
import Data.Maybe
import Data.Aeson

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


type ActivityMap = Map ActivityId Activity
type NodeMap = Map NodeId Node
type EdgeMap = Map EdgeId Edge

newtype ActivityId = ActivityId { getActivityId :: Text }
  deriving ( Show, Generic, Eq, Ord)
newtype NodeId = NodeId { getNodeId :: Int }
  deriving ( Show, Generic, Eq, Ord)
newtype EdgeId = EdgeId { getEdgeId :: Int }
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

instance FromJSONKey NodeId where
  fromJSONKey = fmap NodeId fromJSONKey 
instance ToJSONKey NodeId where
  toJSONKey = contramap getNodeId toJSONKey
instance FromJSON NodeId where
  parseJSON = fmap NodeId . parseJSON
instance ToJSON NodeId where
  toJSON     = toJSON . getNodeId
  toEncoding = toEncoding . getNodeId

instance FromJSONKey EdgeId where
  fromJSONKey = fmap EdgeId fromJSONKey 
instance ToJSONKey EdgeId where
  toJSONKey = contramap getEdgeId toJSONKey
instance FromJSON EdgeId where
  parseJSON = fmap EdgeId . parseJSON
instance ToJSON EdgeId where
  toJSON     = toJSON . getEdgeId
  toEncoding = toEncoding . getEdgeId


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
  , nodeEarliest :: Int
  , nodeLatest :: Int
  } deriving (Generic, FromJSON, ToJSON)

data Edge =
  Edge
  { edgeActivities :: [ActivityId]
  , edgeFrom :: NodeId
  , edgeTo :: NodeId
  , edgeEarliestStart :: Int
  , edgeEarliestFinish :: Int
  , edgeLatestStart :: Int
  , edgeLatestFinish :: Int
  } deriving (Generic, FromJSON, ToJSON)

type DepMap = Map ActivityId (Set ActivityId)
type NextMap = Map (Set ActivityId) (Set ActivityId)
type PrevMap = Map (Set ActivityId) (Set (Set ActivityId, Maybe ActivityId))

depMap :: ActivityMap -> DepMap
depMap as = fmap deps as
  where deps a = foldr S.union (actDependsOn a)
          $ map (\a -> deps $ as M.! a)
          $ S.toList $ actDependsOn a

depGroups :: ActivityMap -> DepMap -> Set (Set ActivityId)
depGroups as ds = addIntersections $ S.insert finish deps
  where deps = S.fromList $ M.elems ds
        finish = M.keysSet as

addIntersections :: Set (Set ActivityId) -> Set (Set ActivityId)
addIntersections ds = S.fromList $ concatMap intersections $ S.toList ds
  where intersections d = map (S.intersection d) $ S.toList ds

nextMap :: ActivityMap -> DepMap -> NextMap
nextMap as ds = foldr (\(k,v) -> M.insertWith S.union v (S.singleton k)) (M.singleton finish S.empty) (M.toList ds)
  where finish = M.keysSet as

nameNodes :: Set (Set ActivityId) -> Map (Set ActivityId) NodeId
nameNodes ds = M.fromList $ zip (S.toList ds) (map NodeId [1..])

actEdges :: DepMap -> Set (Set ActivityId) -> Map ActivityId (Set ActivityId, Set ActivityId)
actEdges ds ns = M.mapWithKey edge ds
  where edge a from = (from, to)
          where done = S.insert a from
                tos = filter (done `S.isSubsetOf`) $ S.toList ns
                to = head $ filter (\c -> not (any (`S.isProperSubsetOf` c) tos)) tos

prevMap :: Map ActivityId (Set ActivityId, Set ActivityId) -> Set (Set ActivityId) -> PrevMap
prevMap es ns = M.fromSet prev ns
  where prev n = acts `S.union` S.fromList (map (\n -> (n, Nothing)) dummies)
          where acts = fromMaybe S.empty $ M.lookup n dests
                froms = filter (\n -> not (S.null n) && n `S.isSubsetOf` missing) $ S.toList ns
                dummies = filter (\n -> not (any (n `S.isProperSubsetOf`) froms)) froms
                missing = n `S.difference` (foldr S.union S.empty $ map (\(f, Just a) -> S.insert a f) $ S.toList acts)
        dests = foldr (\(a,(f,t)) -> M.insertWith S.union t (S.singleton (f, Just a))) M.empty $ M.toList es

edgeMap :: NodeMap -> ActivityMap -> PrevMap -> Map (Set ActivityId) NodeId -> EdgeMap
edgeMap nodes acts ps ns = M.fromList $ zip (map EdgeId [1..])
  $ map (\(t,(f,a)) -> Edge { edgeFrom = ns M.! f
                            , edgeTo = ns M.! t
                            , edgeActivities = maybeToList a
                            , edgeEarliestStart = nodeEarliest (nodes M.! (ns M.! f))
                            , edgeEarliestFinish = nodeEarliest (nodes M.! (ns M.! f)) + maybe 0 (\a -> actDuration (acts M.! a)) a
                            , edgeLatestStart = nodeLatest (nodes M.! (ns M.! t)) - maybe 0 (\a -> actDuration (acts M.! a)) a
                            , edgeLatestFinish = nodeLatest (nodes M.! (ns M.! t))
                            })
  $ concatMap (\(t,es) -> map (\e -> (t,e)) $ S.toList es)
  $ M.toList ps

nodeMap :: Map (Set ActivityId) NodeId -> PrevMap -> ActivityMap -> NodeMap
nodeMap ns prevs acts = M.fromList $ map makeNode (M.toList ns)
  where makeNode (n,id) = (id, Node
                            { nodePrev = []
                            , nodeNext = []
                            , nodeLatest = latest n
                            , nodeEarliest = earliest n
                            })
        expected = earliest $ M.keysSet acts
        earliest n = foldr max 0
          $ map (\(n,a) -> earliest n + maybe 0 (\a -> actDuration (acts M.! a)) a)
          $ S.toList (prevs M.! n)
        latest n = foldr min expected
          $ map (\(n,a) -> latest n - maybe 0 (\a -> actDuration (acts M.! a)) a)
          $ S.toList (nexts M.! n)
        nexts = foldr (\(f,v) -> M.insertWith S.union f (S.singleton v)) (M.singleton (M.keysSet acts) S.empty)
          $ concatMap (\(t,ps) -> map (\(f,a) -> (f,(t,a))) (S.toList ps))
          $ M.toList prevs
