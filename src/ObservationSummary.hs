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
    f path (Node l childrenD _childrenL) = do -- FIXME
      let name = pathToNodeName path
      GraphViz.node name [GraphViz.toLabel (show l)]
      forM_ (IntMap.toList childrenD) $ \(d, ch) -> do
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

deriveTrivialDisequalities' :: ObservationSummary -> Set (Seq Action, Seq Action)
deriveTrivialDisequalities' t = Set.fromList $ do
  ((p1,l1),(p2,l2)) <- pairs [check s $ (s, l) | (p, l :: RoomLabel) <- toList t, let s = planToSeq p]
  guard $ l1 /= l2
  assert (p1 < p2) $ pure (p1, p2)
  where
    check s x
      | any isAlterLabel s = error "label altering is not supported yet"
      | otherwise = x
    isAlterLabel (AlterLabel _) = True
    isAlterLabel (PassDoor _) = False

saturateDisequalities :: Set (Seq Action, Seq Action) -> Set (Seq Action, Seq Action)
saturateDisequalities xs = loop xs xs
  where
    loop :: Set (Seq Action, Seq Action) -> Set (Seq Action, Seq Action) -> Set (Seq Action, Seq Action)
    loop current added
      | Set.null added = current
      | otherwise = loop (current `Set.union` new) (new `Set.difference` current)
      where
        new = Set.fromList $ do
          (p1, p2) <- Set.toList added
          case (p1, p2) of
            (p1' Seq.:|> PassDoor d1, p2' Seq.:|> PassDoor d2) | d1 == d2 ->
              -- 子が異なるなら同じドアを通って子に来ることになった親も異なる
              pure $ if (p1' < p2') then (p1', p2') else (p2', p1')
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

-- | 観測結果等をTrie木にまとめたもの
--
-- * 1つ目のIntMapは、ドアを開けた時の続き (キーはDoor)
--
-- * 2つ目のIntMapは、ラベルを書き換えた時の続き (キーはRoomLabel)
data Trie a = Node !a (IntMap (Trie a)) (IntMap (Trie a))
  deriving (Show, Eq)

instance Functor Trie where
  fmap f (Node l childrenD childrenL) = Node (f l) (fmap (fmap f) childrenD) (fmap (fmap f) childrenL)

fromObservation :: Plan -> [a] -> Trie a
fromObservation plan result = fromObservation' (parsePlan plan) result

fromObservation' :: ParsedPlan -> [a] -> Trie a
fromObservation' [] [l] = Node l IntMap.empty IntMap.empty
fromObservation' (PassDoor d : plan') (x : xs) = Node x (IntMap.singleton d (fromObservation' plan' xs)) IntMap.empty
fromObservation' (AlterLabel l : plan') (x : xs) = Node x IntMap.empty (IntMap.singleton l (fromObservation' plan' xs))
fromObservation' _ _ = undefined

union :: (Eq a, Show a) => Trie a -> Trie a -> Trie a
union (Node l1 childrenD1 childrenL1) (Node l2 childrenD2 childrenL2)
  | l1 /= l2 = error ("label mismatch: " ++ show l1 ++ " /= " ++ show l2)
  | otherwise = Node l1 (IntMap.unionWith union childrenD1 childrenD2) (IntMap.unionWith union childrenL1 childrenL2)

unions :: (Eq a, Show a) => [Trie a] -> Trie a
unions = foldl1' union

fromObservations :: (Eq a, Show a) => [(Plan, [a])] -> Trie a
fromObservations = unions . map (uncurry fromObservation)

toObservations :: Trie a -> [(Plan, [a])]
toObservations = f Seq.empty Seq.empty
  where
    f hist ls (Node label childrenD childrenL)
      | IntMap.null childrenD && IntMap.null childrenL = pure (seqToPlan hist, F.toList (ls Seq.|> label))
      | otherwise = msum
          [ do (d, node) <- IntMap.toList childrenD
               f (hist Seq.|> PassDoor d) (ls Seq.|> label) node
          , do (l, node) <- IntMap.toList childrenL
               f (hist Seq.|> AlterLabel l) (ls Seq.|> label) node
          ]

toList :: forall a. Trie a -> [(Plan, a)]
toList = f Seq.empty
  where
    f :: Seq Action -> Trie a -> [(Plan, a)]
    f hist (Node label childrenD childrenL) =
      (seqToPlan hist, label) :
      concat [f (hist Seq.|> PassDoor d) ch | (d, ch) <- IntMap.toList childrenD] ++
      concat [f (hist Seq.|> AlterLabel l) ch | (l, ch) <- IntMap.toList childrenL]

lookup :: Plan -> Trie a -> Maybe a
lookup plan t = lookup' (parsePlan plan) t

lookup' :: ParsedPlan -> Trie a -> Maybe a
lookup' [] (Node l _childrenD _childrenL) = Just l
lookup' (PassDoor d : ds) (Node _ childrenD _childrenL) = do
  ch <- IntMap.lookup d childrenD
  lookup' ds ch
lookup' (AlterLabel l : ds) (Node _ _childrenD childrenL) = do
  ch <- IntMap.lookup l childrenL
  lookup' ds ch

keys :: forall a. Trie a -> [Plan]
keys = map seqToPlan . f Seq.empty
  where
    f :: Seq Action -> Trie a -> [Seq Action]
    f hist (Node _ childrenD childrenL) =
      hist :
      concat [f (hist Seq.|> PassDoor d) ch | (d, ch) <- IntMap.toList childrenD] ++
      concat [f (hist Seq.|> AlterLabel l) ch | (l, ch) <- IntMap.toList childrenL]

mapWithKey :: forall a b. (Plan -> a -> b) -> Trie a -> Trie b
mapWithKey f = g Seq.empty
  where
    g :: Seq Action -> Trie a -> Trie b
    g hist (Node label childrenD childrenL) =
      Node (f (seqToPlan hist) label)
        (IntMap.mapWithKey (\d ch -> g (hist Seq.|> PassDoor d) ch) childrenD)
        (IntMap.mapWithKey (\l ch -> g (hist Seq.|> AlterLabel l) ch) childrenL)

-- ------------------------------------------------------------------------

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs

planToSeq :: Plan -> Seq Action
planToSeq = Seq.fromList . parsePlan

seqToPlan :: Seq Action -> Plan
seqToPlan = renderPlan . F.toList

-- ------------------------------------------------------------------------
