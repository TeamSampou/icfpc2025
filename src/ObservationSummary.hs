module ObservationSummary
 (
 -- * Observation summary
   ObservationSummary
 , toDotGraph
 , writePng

 -- * Trie
 , Trie (..)
 , singleton
 , union
 , unions
 , fromList
 , toList
 ) where

import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl1')
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.GraphViz.Attributes as GraphViz
import qualified Data.GraphViz.Commands as GraphViz
import qualified Data.GraphViz.Commands.IO as GraphViz
import qualified Data.GraphViz.Types.Generalised as GraphViz
import qualified Data.GraphViz.Types.Monadic as GraphViz

import Base


-- ------------------------------------------------------------------------

type ObservationSummary = Trie RoomLabel

toDotGraph :: ObservationSummary -> GraphViz.DotGraph Text
toDotGraph t = GraphViz.digraph' $ f [] t
  where
    pathToNodeName :: [Door] -> Text
    pathToNodeName path = T.pack $ "node" ++ concatMap show (reverse path)

    f :: [Door] -> ObservationSummary -> GraphViz.DotM Text Text
    f path (Node l children) = do
      let name = pathToNodeName path
      GraphViz.node name [GraphViz.toLabel (show l)]
      forM_ (IntMap.toList children) $ \(d, ch) -> do
        name' <- f (d : path) ch
        GraphViz.edge name name' [GraphViz.toLabel (show d)]
      pure name

writePng :: FilePath -> ObservationSummary -> IO ()
writePng fname t = void $ GraphViz.runGraphviz (toDotGraph t) GraphViz.Png fname

_test_toDotGraph :: IO ()
_test_toDotGraph = do
  _ <- GraphViz.runGraphviz dg GraphViz.Png "test.png"
  GraphViz.writeDotFile "test.dot" dg
  pure ()
  where
    dg = toDotGraph c
    a = fromList $ zip ["000","123","213","333"] [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]]
    b = fromList $ zip ["4","5","02","03","04","05","31","32","34","35"] [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]
    c = union a b

-- ------------------------------------------------------------------------

data Trie a = Node !a (IntMap (Trie a))
  deriving (Show, Eq)

instance Functor Trie where
  fmap f (Node l children) = Node (f l) (fmap (fmap f) children)

singleton :: Plan -> [a] -> Trie a
singleton [] [l] = Node l (IntMap.empty)
singleton (d : plan') (l : ls) = Node l (IntMap.singleton (read [d]) (singleton plan' ls))
singleton _ _ = undefined

union :: (Eq a, Show a) => Trie a -> Trie a -> Trie a
union (Node l1 children1) (Node l2 children2)
  | l1 /= l2 = error ("label mismatch: " ++ show l1 ++ " /= " ++ show l2)
  | otherwise = Node l1 $ IntMap.unionWith union children1 children2

unions :: (Eq a, Show a) => [Trie a] -> Trie a
unions = foldl1' union

fromList :: (Eq a, Show a) => [(Plan, [a])] -> Trie a
fromList = unions . map (uncurry singleton)

toList :: Trie a -> [(Plan, [a])]
toList = f [] []
  where
    f ds ls (Node l children)
      | IntMap.null children = pure (concat $ map show $ reverse ds, reverse (l : ls))
      | otherwise = do
          (d, node) <- IntMap.toList children
          f (d : ds) (l : ls) node

-- ------------------------------------------------------------------------
