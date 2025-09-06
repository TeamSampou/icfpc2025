module ObservationSummary
 (
 -- * Observation summary
   ObservationSummary
 , toDotGraph
 , writePng
 , deriveDisequalities
 , deriveNontrivialDisequalities
 , deriveTrivialDisequalities

 -- * Trie
 , Trie (..)
 , fromObservation
 , fromObservations
 , union
 , unions
 , toObservations
 , toList
 , lookup
 , keys
 , mapWithKey
 ) where

import Prelude hiding (lookup)

import Control.Exception (assert)
import Control.Monad
import qualified Data.Foldable as F
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl1')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
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
    a = fromObservations $ zip ["000","123","213","333"] [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]]
    b = fromObservations $ zip ["4","5","02","03","04","05","31","32","34","35"] [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]
    c = union a b

-- | Plan(のPrefix)で表される部屋のうち、異なる部屋であることが確定している組み合わせを列挙する
deriveDisequalities :: ObservationSummary -> Set (Plan, Plan)
deriveDisequalities t = Set.map (\(p1,p2) -> (seqToPlan p1, seqToPlan p2)) $ saturateDisequalities (deriveTrivialDisequalities' t)

-- | 'deriveDisequalities' と同様だが、ラベルの違いだけで区別できるような部屋の組は除外する
deriveNontrivialDisequalities :: ObservationSummary -> Set (Plan, Plan)
deriveNontrivialDisequalities t = Set.map (\(p1,p2) -> (seqToPlan p1, seqToPlan p2)) $ saturateDisequalities xs `Set.difference` xs
  where
    xs = deriveTrivialDisequalities' t

-- | ラベルの違いだけで区別できるような部屋の組
deriveTrivialDisequalities :: ObservationSummary -> Set (Plan, Plan)
deriveTrivialDisequalities t = Set.map (\(p1,p2) -> (seqToPlan p1, seqToPlan p2)) $ deriveTrivialDisequalities' t

deriveTrivialDisequalities' :: ObservationSummary -> Set (Seq Door, Seq Door)
deriveTrivialDisequalities' t = Set.fromList $ do
  ((p1,l1),(p2,l2)) <- pairs [(planToSeq p, l) | (p, l :: RoomLabel) <- toList t]
  guard $ l1 /= l2
  assert (p1 < p2) $ pure (p1, p2)

saturateDisequalities :: Set (Seq Door, Seq Door) -> Set (Seq Door, Seq Door)
saturateDisequalities xs = loop xs xs
  where
    loop :: Set (Seq Door, Seq Door) -> Set (Seq Door, Seq Door) -> Set (Seq Door, Seq Door)
    loop current added
      | Set.null new = current
      | otherwise = loop (current `Set.union` new) new
      where
        new = new' `Set.difference` current
        new' = Set.fromList $ do
          (p1, p2) <- Set.toList added
          case (p1, p2) of
            (p1' Seq.:|> d1, p2' Seq.:|> d2) | d1 == d2 -> assert (p1' < p2') [(p1', p2')] -- 子が異なるなら同じドアを通って子に来ることになった親も異なる
            _ -> []

test_deriveNontrivialDisequalities :: [(Plan, Plan, Maybe RoomLabel)]
test_deriveNontrivialDisequalities = [assert (lookup p1 t == lookup p2 t) (p1, p2, lookup p1 t) | (p1,p2) <- Set.toList deqs]
  where
    deqs = deriveNontrivialDisequalities t

    plan = "231025153214112435435242513423043543011400411314240201"

    result :: [RoomLabel]
    result = [0,3,0,1,2,3,0,1,0,0,1,1,0,1,1,0,1,1,0,0,0,1,3,2,3,0,1,1,0,3,0,0,0,0,1,2,1,2,2,2,1,2,3,2,2,2,1,0,1,0,1,0,1,0,1]

    t = fromObservation plan result

-- ------------------------------------------------------------------------

data Trie a = Node !a (IntMap (Trie a))
  deriving (Show, Eq)

instance Functor Trie where
  fmap f (Node l children) = Node (f l) (fmap (fmap f) children)

fromObservation :: Plan -> [a] -> Trie a
fromObservation [] [l] = Node l (IntMap.empty)
fromObservation (d : plan') (l : ls) = Node l (IntMap.singleton (read [d]) (fromObservation plan' ls))
fromObservation _ _ = undefined

union :: (Eq a, Show a) => Trie a -> Trie a -> Trie a
union (Node l1 children1) (Node l2 children2)
  | l1 /= l2 = error ("label mismatch: " ++ show l1 ++ " /= " ++ show l2)
  | otherwise = Node l1 $ IntMap.unionWith union children1 children2

unions :: (Eq a, Show a) => [Trie a] -> Trie a
unions = foldl1' union

fromObservations :: (Eq a, Show a) => [(Plan, [a])] -> Trie a
fromObservations = unions . map (uncurry fromObservation)

toObservations :: Trie a -> [(Plan, [a])]
toObservations = f Seq.empty Seq.empty
  where
    f ds ls (Node l children)
      | IntMap.null children = pure (seqToPlan ds, F.toList (ls Seq.|> l))
      | otherwise = do
          (d, node) <- IntMap.toList children
          f (ds Seq.|> d) (ls Seq.|> l) node

toList :: forall a. Trie a -> [(Plan, a)]
toList = f Seq.empty
  where
    f :: Seq Door -> Trie a -> [(Plan, a)]
    f hist (Node l children) = (seqToPlan hist, l) : concat [f (hist Seq.|> d) ch | (d, ch) <- IntMap.toList children]

lookup :: Plan -> Trie a -> Maybe a
lookup [] (Node l _children) = Just l
lookup (d : ds) (Node _ children) = do
  ch <- IntMap.lookup (read [d]) children
  lookup ds ch

keys :: forall a. Trie a -> [Plan]
keys = map seqToPlan . f Seq.empty
  where
    f :: Seq Door -> Trie a -> [Seq Door]
    f hist (Node _ children) = hist : concat [f (hist Seq.|> d) ch | (d, ch) <- IntMap.toList children]

mapWithKey :: forall a b. (Plan -> a -> b) -> Trie a -> Trie b
mapWithKey f = g Seq.empty
  where
    g :: Seq Door -> Trie a -> Trie b
    g hist (Node l children) = Node (f (seqToPlan hist) l) (IntMap.mapWithKey (\d ch -> g (hist Seq.|> d) ch) children)

-- ------------------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

planToSeq :: Plan -> Seq Door
planToSeq = Seq.fromList . map (\c -> read [c])

seqToPlan :: Seq Door -> Plan
seqToPlan = concat . map show . F.toList

-- ------------------------------------------------------------------------
