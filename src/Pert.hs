{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards
           , OverloadedStrings #-}

module Pert
  ( ActivityId(..), NodeId(..), EdgeId(..)
  , Activity(..), Node(..), Edge(..)
  , ActivityMap, NodeMap, EdgeMap
  , depGroups, depMap, nextMap, actEdges
  , prevMap, edgeMap, nameNodes
  ) where

import GHC.Generics

import Control.Applicative

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

newtype ActivityId = ActivityId Text
  deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)
newtype NodeId = NodeId Int
  deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)
newtype EdgeId = EdgeId Int
  deriving (Show, Generic, FromJSON, ToJSON, Eq, Ord)

data Activity =
  Activity
  { actDescr :: Text
  , actDuration :: Int
  , actDependsOn :: Set ActivityId
  } deriving (Generic, FromJSON, ToJSON)

data Node =
  Node
  { nodeName :: Int
--  , nodeEarliest :: Int
--  , nodeLatest :: Int
  , nodePrev :: [EdgeId]
  , nodeNext :: [EdgeId]
  } deriving (Generic, FromJSON, ToJSON)

data Edge =
  Edge
  { edgeActivities :: [ActivityId]
  , edgeFrom :: NodeId
  , edgeTo :: NodeId
--  , edgeEarliestStart :: Int
--  , edgeEarliestFinish :: Int
--  , edgeLatestStart :: Int
--  , edgeLatestFinish :: Int
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
depGroups as ds = S.insert finish deps
  where deps = S.fromList $ M.elems ds
        finish = M.keysSet as

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

edgeMap :: PrevMap -> Map (Set ActivityId) NodeId -> EdgeMap
edgeMap ps ns = M.fromList $ zip (map EdgeId [1..])
  $ map (\(t,(f,a)) -> Edge { edgeFrom = ns M.! f
                            , edgeTo = ns M.! t
                            , edgeActivities = maybeToList a })
  $ concatMap (\(t,es) -> map (\e -> (t,e)) $ S.toList es)
  $ M.toList ps


-- edges :: ActivityMap -> NodeMap -> EdgeMap
-- edges acts nodes = M.fromList $ map edge acts
--   where edge act@Activity{..} = ((from,to), Edge{..})
--           where edgeActivity = act
--                 (from,edgeFrom) = head -- todo: dummies
--                   $ filter (\(k,v) -> actDependsOn == k -- to catch empty
--                              || not (S.disjoint actDependsOn k))
--                   $ M.toList nodes
--                 (to,edgeTo) = head
--                   $ filter (\(k,v) -> (S.member actId k))
--                   $ M.toList nodes

-- nodes :: Set (Set Text) -> EdgeMap -> NodeMap
-- nodes deps edges = M.fromList $ map node $ zip [1..] $ S.toList deps
--   where node (i,dep) = (dep,Node{..})
--           where nodePrev = map snd
--                   $ filter (\((f,_),v) -> f == dep)
--                   $ M.toList edges
--                 nodeNext = map snd
--                   $ filter (\((_,t),v) -> t == dep)
--                   $ M.toList edges
--                 nodeId = i

-- graph :: [Activity] -> (NodeMap, EdgeMap)
-- graph acts = (ns,es) -- head $ start $ M.toList ns
--   where gs = depGroups (map actDependsOn acts)
--         f = S.difference (S.fromList $ map actId acts)
--           $ foldr S.union S.empty $ S.toList gs
--         ns = nodes (gs <> S.singleton f) es
--         es = edges acts ns
--         start = map snd . filter (\(k,v) -> S.null k)

-- walk :: Node -> IO ()
-- walk Node{nodeNext = [], ..} = T.putStrLn $ T.pack (show nodeId)
-- walk Node{nodeNext = (Edge{..}:_), ..} = do
--   T.putStrLn $ T.pack (show nodeId)
--   T.putStrLn $ " -- " <> actId edgeActivity <> " --> "
--   walk edgeTo
