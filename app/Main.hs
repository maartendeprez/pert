{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Pert

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.List
import Data.Maybe

activities :: Map ActivityId Activity
activities = M.fromList
  [ ( ActivityId "A", Activity
    { actDescr = "A"
    , actDuration = 1
    , actDependsOn = S.empty
    }
  )
  , ( ActivityId "B", Activity
      { actDescr = "B"
      , actDuration = 5
      , actDependsOn = S.empty
      }
    )
  , ( ActivityId "C", Activity
      { actDescr = "C"
      , actDuration = 5
      , actDependsOn = S.singleton $ ActivityId "B"
      }
    )
  , ( ActivityId "D", Activity
      { actDescr = "D"
      , actDuration = 3
      , actDependsOn = S.singleton $ ActivityId "A"
      }
    )
  , ( ActivityId "E", Activity
      { actDescr = "E"
      , actDuration = 2
      , actDependsOn = S.fromList
          [ ActivityId "C"
          , ActivityId "D"
          ]
      }
    )
  , ( ActivityId "F", Activity
      { actDescr = "F"
      , actDuration = 2
      , actDependsOn = S.fromList
          [ ActivityId "C"
          ]
      }
    )
  ]

main :: IO ()
main = do
  T.putStrLn $ "Nodes:\n"
    <> T.unlines (map (\(d, NodeId n) -> T.pack (show n) <> ": " <> T.intercalate ", " (map (\(ActivityId a) -> a) $ S.toList d)) $ M.toList names)
  T.putStrLn $ ""
  T.putStrLn $ "Edges:\n" <> T.unlines (map (\(EdgeId i,e) -> T.pack (show i) <> ": " <> nodeName' (edgeFrom e) <> " -(" <> maybe "0" (\(ActivityId t) -> t) (listToMaybe $ edgeActivities e) <> ")-> " <> nodeName' (edgeTo e)) $ M.toList edges)
  where deps = depMap activities
        nodes = depGroups activities deps
        nexts = nextMap activities deps
        acts = actEdges deps nodes
        prevs = prevMap acts nodes
        names = nameNodes nodes
        edges = edgeMap prevs names
        nodeName n = case names M.! n of NodeId n -> T.pack (show n)
        nodeName' n = case n of NodeId n -> T.pack (show n)
  -- $ map actDependsOn (M.elems activities)
