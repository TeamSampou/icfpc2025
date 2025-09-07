module Graph
  ( DiGraph
  , fromTrie
  , enumGraph
  , fromLayout
  , toLayout
  , toDotGraph
  , writePng
  ) where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (groupBy, intercalate, nubBy, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.GraphViz.Attributes as GraphViz
import qualified Data.GraphViz.Commands as GraphViz
import qualified Data.GraphViz.Commands.IO as GraphViz
import qualified Data.GraphViz.Types.Generalised as GraphViz
import qualified Data.GraphViz.Types.Monadic as GraphViz

import Base
import ObservationSummary (ObservationSummary, Trie)
import qualified ObservationSummary as Trie

-- | 有向グラフ表現
--
-- * Vector のインデックスはグラフの頂点のID('RoomIndex')
--
-- * 要素はその頂点のラベルと 出る側のドア番号でラベル付けされた出力エッジ
--
-- * 入る側の部屋のドア番号の情報は付与されていないので注意
type DiGraph = Vector (RoomLabel, IntMap RoomIndex)

-- | RoomLabel に加えて RoomIndex を決定した Trie を元に有向グラフ表現に変換し、最初の部屋の番号とともに返す
--
-- 実装効率悪そう
fromTrie :: Int -> Trie (RoomLabel, RoomIndex) -> (DiGraph, RoomIndex)
fromTrie numRooms trie@(Trie.Node (_, startingRoom) _ _) = (V.generate numRooms g, startingRoom)
  where
    g :: Int -> (RoomLabel, IntMap RoomIndex)
    g i = assert (i == roomId) $ (label, fmap (\(Trie.Node (label', _) _ _) -> label') childrenD)
      where
        Trie.Node (label, roomId) childrenD _ = merged IntMap.! i

    -- RoomIndex をキーにして情報を集約したもの (実装効率悪そう)
    merged :: IntMap (Trie (RoomLabel, RoomIndex))
    merged = f trie
      where
        f :: Trie (RoomLabel, RoomIndex) -> IntMap (Trie (RoomLabel, RoomIndex))
        f t@(Trie.Node (_, roomId) childrenD childrenL)
          | not (IntMap.null childrenL) = error "label altering is not supported yet"
          | otherwise = IntMap.unionsWith Trie.union $ IntMap.singleton roomId t : map f (IntMap.elems childrenD)

-- | 与えられた部屋数と 'ObservationSummary' から、それに整合的なグラフ構造を列挙する
enumGraph :: Int -> ObservationSummary -> [(DiGraph, RoomIndex)]
enumGraph numRooms trie = do
  (startingRoom, g) <- runStateT (f trie) Seq.empty
  pure (V.fromList (F.toList g), startingRoom)
  where
    f :: ObservationSummary -> StateT (Seq (RoomLabel, IntMap RoomIndex)) [] RoomIndex
    f (Trie.Node label childrenD childrenL) = do
      unless (IntMap.null childrenL) $ error "label altering is not supported yet"
      outs <- mapM f childrenD
      g <- get
      msum $
        -- 既存のノードにマージするパターン
        [ do guard $ label == label'
             guard $ and $ IntMap.intersectionWith (==) outs outs'
             put $ Seq.update idx (label, IntMap.union outs outs') g
             return idx
        | (idx, (label', outs')) <- zip [0..] (F.toList g)
        ] ++
        -- 新しいノードを作るパターン
        [ do let idx = Seq.length g
             guard $ idx < numRooms
             put $ g Seq.|> (label, outs)
             return idx
        ]

fromLayout :: Layout -> (DiGraph, RoomIndex)
fromLayout (labels, startingRoom, ds) = (V.fromList m, startingRoom)
  where
    m :: [(RoomLabel, IntMap RoomIndex)]
    m = [(label, IntMap.fromList(zip[0..5](map snd doors)) ) | (label, doors) <-zip labels doorss]

    doorss :: [[((RoomIndex, Door), RoomIndex)]]
    doorss = groupBy((==)`on`fst.fst) $ nubBy((==)`on`fst) $ sort $ concat[[((r1,d1),r2), ((r2,d2),r1)] | ((r1,d1),(r2,d2)) <-ds]

-- | 有向グラフ表現の 'Layout' への変換
toLayout :: (DiGraph, RoomIndex) -> Layout
toLayout (g, startingRoom) =
  ( map fst (V.toList g)
  , startingRoom
  , concat
    [ if room1 == room2 then do
        d <- IntSet.toList $ Map.findWithDefault IntSet.empty (room1, room2) cs
        pure ((room1,d), (room2,d))
      else do
        let ds1 = Map.findWithDefault IntSet.empty (room1, room2) cs
            ds2 = Map.findWithDefault IntSet.empty (room2, room1) cs
        unless (IntSet.size ds1 == IntSet.size ds2) $ error "not symmetric"
        zip (map (\d -> (room1, d)) (IntSet.toList ds1)) (map (\d -> (room2, d)) (IntSet.toList ds2))
    | room1 <- [0..numRooms-1]
    , room2 <- [room1..numRooms-1]
    ]
  )
  where
    numRooms = V.length g

    -- (room1, room2) ↦ { d | room1 のドア d が room2 に繋がっている }
    cs :: Map (RoomIndex,RoomIndex) IntSet
    cs = Map.fromListWith IntSet.union [((room1, room2), IntSet.singleton d) | (room1, (_, outs)) <- zip [0..] (V.toList g), (d, room2) <- IntMap.toList outs]

toDotGraph :: (DiGraph, RoomIndex) -> GraphViz.DotGraph Text
toDotGraph (g, startingRoom) = GraphViz.digraph' $ do
  forM_ (zip [0..] (V.toList g)) $ \(i, (label, outs)) -> do
    let name1 = indexToNodeName i
    GraphViz.node name1 $ [GraphViz.toLabel (show label)] ++ [GraphViz.shape GraphViz.DoubleCircle | i == startingRoom]
    let tmp = IntMap.fromListWith IntSet.union [(j, IntSet.singleton d) | (d,j) <- IntMap.toList outs]
    forM_ (IntMap.toList tmp) $ \(j, ds) -> do
      let name2 = indexToNodeName j
      GraphViz.edge name1 name2 [GraphViz.toLabel (intercalate "," $ map show $ IntSet.toList ds)]
  where
    indexToNodeName :: RoomIndex -> Text
    indexToNodeName i = T.pack $ "node" ++ show i

writePng :: FilePath -> (DiGraph, RoomIndex) -> IO ()
writePng fname g = void $ GraphViz.runGraphviz (toDotGraph g) GraphViz.Png fname


{-
probatioで手動で試した以下のデータの場合。

ghci> initClient
initClient
ghci> select "probatio"
"probatio"
ghci> explore ["000","123","213","333"]
([[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]],5)
ghci> explore ["4","5","02","03","04","05","31","32","34","35"]
([[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]],16)
ghci> guess ([0,1,2], 0, [((0,0),(1,2)), ((0,1),(0,1)), ((0,2),(1,5)), ((0,3),(2,0)), ((0,4),(2,2)), ((0,5),(2,3)), ((1,0),(2,1)), ((1,1),(1,4)), ((1,3), (2,5)), ((2,4),(2,4)), ((2,5),(1,3))])
True
-}

_test_fromTrie :: (DiGraph, RoomIndex)
_test_fromTrie = fromTrie 3 $ fmap (\label -> (label, label)) c
  where
    a = Trie.fromObservations $ zip ["000","123","213","333"] [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]]
    b = Trie.fromObservations $ zip ["4","5","02","03","04","05","31","32","34","35"] [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]
    c = Trie.union a b

_test_toLayout :: Layout
_test_toLayout = toLayout _test_fromTrie

_test_writePng :: IO ()
_test_writePng = writePng "test.png" _test_fromTrie

-- primus の場合の例
_test_enumGraph :: IO ()
_test_enumGraph = writePng "test.png" $ head (enumGraph 6 t)
  where
    plan = "021320403505044123550520034431312210134541025332505010033554343013423011254052531011533004340304253205132534"
    result :: [RoomLabel]
    result = [0,1,0,0,2,0,2,1,1,1,1,1,1,1,1,2,0,0,2,0,0,2,0,0,2,0,2,1,2,0,2,0,2,0,0,2,0,0,2,1,3,3,3,1,0,2,0,1,3,1,0,2,0,0,1,1,1,1,1,3,3,3,3,3,1,3,3,3,1,1,0,0,0,0,2,1,1,1,0,2,0,2,0,0,0,2,0,1,0,1,2,0,2,0,2,0,1,3,1,1,3,1,3,3,3,1,3,3,3]
    t = Trie.fromObservation plan result
