module Graph
  ( DiGraph
  , fromTrie
  ) where

import Control.Exception (assert)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Vector (Vector)
import qualified Data.Vector as V

import Base
import ObservationSummary (Trie)
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
fromTrie numRooms trie@(Trie.Node (_, initRoomId) _) = (V.generate numRooms g, initRoomId)
  where
    g :: Int -> (RoomLabel, IntMap RoomIndex)
    g i = assert (i == roomId) $ (label, fmap (\(Trie.Node (label', _) _) -> label') children)
      where
        Trie.Node (label, roomId) children = merged IntMap.! i

    -- RoomIndex をキーにして情報を集約したもの (実装効率悪そう)
    merged :: IntMap (Trie (RoomLabel, RoomIndex))
    merged = f trie
      where
        f :: Trie (RoomLabel, RoomIndex) -> IntMap (Trie (RoomLabel, RoomIndex))
        f t@(Trie.Node (_, roomId) children) = IntMap.unionsWith Trie.union $ IntMap.singleton roomId t : map f (IntMap.elems children)

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
ghci> guess ([0,1,2], 0, [((0,0),(1,2)), ((0,1),(0,1)), ((0,2),(1,5)), ((0,3),(2,0)), ((0,4),(2,2)), ((0,5),((2,3))), ((1,0),(2,1)), ((1,1),(1,4)), ((1,3), (2,5)), ((2,4),(2,4)), ((2,5),(1,3))])
True
-}
_test :: (DiGraph, RoomIndex)
_test = fromTrie 3 $ fmap (\label -> (label, label)) c
  where
    a = Trie.fromList $ zip ["000","123","213","333"] [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]]
    b = Trie.fromList $ zip ["4","5","02","03","04","05","31","32","34","35"] [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]
    c = Trie.union a b
