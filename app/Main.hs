{-# LANGUAGE OverloadedStrings, RecordWildCards, QuasiQuotes #-}

module Main where

import Pert

import Text.Hamlet (shamlet, Html)
import Text.Julius (julius, renderJavascriptUrl)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.Aeson (encode, decode, object, (.=), Value(..))

import Data.Function (on)
import qualified Data.Vector as V

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Text as T

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.ByteString.Lazy as BL

import Data.List
import Data.Maybe

cypage :: Value -> EdgeMap -> ActivityMap -> Html
cypage elems edges acts = [shamlet|
$doctype 5
<html>
   <head>
      <title>PERT
      <meta charset="UTF-8">
      <script src="https://unpkg.com/cytoscape">
      <script src="https://unpkg.com/dagre/dist/dagre.js">
      <script src="https://unpkg.com/cytoscape-dagre">
      <script src="https://unpkg.com/@popperjs/core">
      <script src="https://unpkg.com/cytoscape-popper">
   <body>
      <div #cy style="position: absolute; top: 10px; bottom: 10px; left: 10px; right: 10px;">
      <script>#{cyscript elems}
      <table style="position: absolute; top: 10px; bottom: 10px; left: 67%; right: 10px;">
         <thead>
            <th>Activity
            <th>Duration
            <th>ES
            <th>LS
            <th>LS
            <th>LF
            <th>Slack
         $forall Edge{..} <- nonDummies edges
           <tr>
               <td>#{actNames acts edgeActivities}
               <td>#{duration acts edgeActivities}
               <td>#{edgeEarliestStart}
               <td>#{edgeLatestStart}
               <td>#{edgeEarliestFinish}
               <td>#{edgeLatestFinish}
               <td>#{edgeLatestFinish - edgeEarliestFinish}
|]

nonDummies :: EdgeMap -> [Edge]
nonDummies = sortBy (compare `on` ((\(ActivityId t) -> t) . head . edgeActivities)) . filter (not . null . edgeActivities) . M.elems
  
actNames :: ActivityMap -> [ActivityId] -> T.Text
actNames acts = T.intercalate ", " . map (\(ActivityId t) -> t) 
  
duration :: ActivityMap -> [ActivityId] -> Int
duration acts = foldr max 0 . map (actDuration . (acts M.!)) 
  
cyscript :: Value -> Html
cyscript elems = preEscapedToMarkup $ renderJavascriptUrl undefined
  $ [julius|
var cy = cytoscape({
   container: document.getElementById('cy'),
   elements: #{elems},
   style: [
      {
         selector: 'node',
            style: {
               label: 'data(id)',
               'text-valign': 'center',
               'text-halign': 'center'
            }
         },
      {
         selector: 'edge',
         style: {
            'curve-style': 'straight',
            'target-arrow-shape': 'triangle',
            'label': 'data(activity)'
            //'edge-text-rotation': 'autorotate',
         }
      },
      {
         selector: 'edge[?critical]',
         style: {
            'line-color': 'blue',
            'target-arrow-color': 'blue',
         }
      }
   ],
   layout: {
      name: 'dagre',
      rankDir: 'LR',
      spacingFactor: 2,
   }
});
const poppers = cy.nodes().map(node => {
   const popper = node.popper({
      content: () => {
         let div = document.createElement('div');
         div.innerHTML = '(' + node.data()['te'].toString()
            + ' - ' + node.data()['tl'].toString() + ')';
         document.body.appendChild(div);
         return div;
      },
      popper: {}
   });
   node.on('position', popper.update);
   return popper;
});
cy.on('pan zoom resize', () => poppers.forEach(popper => popper.update()));
|]

main :: IO ()
main = do
  Just activities <- decode <$> BL.getContents

  let deps = depMap activities
      nodes = depGroups activities deps
      nexts = nextMap activities deps
      acts = actEdges deps nodes
      prevs = prevMap acts nodes
      names = nameNodes nodes
      edges = edgeMap nodes' activities prevs names
      nodes' = nodeMap names prevs activities
      nodeName n = case names M.! n of NodeId n -> T.pack (show n)
      nodeName' n = case n of NodeId n -> T.pack (show n)
      cyelems = Array $ V.fromList
        $ map (\(NodeId i, n) -> object
                ["data" .= object
                  [ "id" .= T.pack (show i)
                  , "te" .= nodeEarliest n
                  , "tl" .= nodeLatest n
                  ]
                ]) (M.toList nodes')
        <> map (\(EdgeId i, e) -> object
                 ["data" .= object
                   [ "id" .= ("edge-" <> T.pack (show i))
                   , "source" .= nodeName' (edgeFrom e)
                   , "target" .= nodeName' (edgeTo e)
                   , "activity" .= case edgeActivities e of
                       [] -> "0"
                       as -> T.intercalate ", " $ map (\(ActivityId t) -> t) as
                   , "duration" .= duration activities (edgeActivities e)
                   , "slack" .= (edgeLatestFinish e - edgeEarliestFinish e)
                   , "critical" .= (edgeLatestFinish e == edgeEarliestFinish e)
                   ]
                 ]) (M.toList edges)

  TL.putStrLn $ renderHtml $ cypage cyelems edges activities
