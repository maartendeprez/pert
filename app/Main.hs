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

cypage :: Graph -> Html
cypage g@Graph{..} = [shamlet|
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
      <script>#{cyscript g}
      <div style="position: absolute; top: 10px; bottom: 10px; left: 67%; right: 10px;">
         <p>Expected completion = #{round' grExpected}<br/>
            σ² = #{round' grExpectedSigmaSq}
         <table>
            <thead>
               <th>Activity
               <th>Duration
               <th>σ²
               <th>ES
               <th>LS
               <th>EF
               <th>LF
               <th>Slack
            $forall Edge{..} <- nonDummies grEdges
               <tr>
                  <td>#{actNames grActivities edgeActivities}
                  <td>#{round' edgeDuration}
                  <td>#{round' edgeDurationSigmaSq}
                  <td>#{round' edgeEarliestStart}
                  <td>#{round' edgeLatestStart}
                  <td>#{round' edgeEarliestFinish}
                  <td>#{round' edgeLatestFinish}
                  <td>#{round' edgeSlack}
|]

cyscript :: Graph -> Html
cyscript g = preEscapedToMarkup $ renderJavascriptUrl undefined
  $ [julius|
var cy = cytoscape({
   container: document.getElementById('cy'),
   elements: #{cyelems g},
   style: [
      {
         selector: 'node',
            style: {
               label: 'data(n)',
               'text-valign': 'center',
               'text-halign': 'center'
            }
         },
      {
         selector: 'edge',
         style: {
            'curve-style': 'straight',
            'target-arrow-shape': 'triangle',
            'label': 'data(activity)',
            'text-margin-y': '15',
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
         const te = node.data()['te'];
         const tl = node.data()['tl'];
         div.innerHTML = '(' + (te === tl ? te.toString()
            : te.toString() + ' - ' + tl.toString()) + ')';
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

cyelems :: Graph -> Value
cyelems Graph{..} = Array $ V.fromList
  $ map (\(_, Node{..}) -> object
          ["data" .= object
            [ "id" .= ("node-" <> T.pack (show nodeName))
            , "n" .= T.pack (show nodeName)
            , "te" .= round' nodeEarliest
            , "tl" .= round' nodeLatest
            ]
          ]) (M.toList grNodes)
  <> map (\(i, Edge{..}) -> object
           ["data" .= object
             [ "id" .= ("edge-" <> T.pack (show i))
             , "source" .= ("node-" <> T.pack (show $ nodeName $ grNodes M.! edgeFrom))
             , "target" .= ("node-" <> T.pack (show $ nodeName $ grNodes M.! edgeTo))
             , "activity" .= case edgeActivities of
                 [] -> "0"
                 as -> T.intercalate ", " $ map (\(ActivityId t) -> t) as
             , "duration" .= round' edgeDuration
             , "sigmasq" .= round' edgeDurationSigmaSq
             , "slack" .= round' edgeSlack
             , "critical" .= (abs (edgeLatestFinish - edgeEarliestFinish)
                              < 0.01 )
             ]
           ]) (zip [1..] $ M.elems grEdges)

nonDummies :: EdgeMap -> [Edge]
nonDummies = sortBy (compare `on` ((\(ActivityId t) -> t) . head . edgeActivities)) . filter (not . null . edgeActivities) . M.elems
  
actNames :: ActivityMap -> [ActivityId] -> T.Text
actNames acts = T.intercalate ", " . map (\(ActivityId t) -> t) 

duration :: ActivityMap -> [ActivityId] -> Double
duration acts = foldr max 0 . map (actDuration . (acts M.!))

round' :: Double -> Double
round' n = fromIntegral (round $ n * 100) / 100 

main :: IO ()
main = do
  Just activities <- decode <$> BL.getContents
  let g = graph activities
  TL.putStrLn $ renderHtml $ cypage g
