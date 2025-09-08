{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import Control.Monad (when, replicateM)
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.List ( find, groupBy, nubBy, sort, sortBy, transpose )
import Data.Maybe (catMaybes, fromJust, maybeToList)
import Data.Tuple (swap)
import Debug.Trace ( traceShow )
import System.Environment ( getArgs, getProgName )
import qualified System.Random.MWC as Rand

import Base
import Client ( initClient, select, explore, guess )


main :: IO ()
main =  do
    _progName <- getProgName
    problem:limit:_   <- getArgs
    initClient
    _ <- select problem
    answer <- solve (read limit)
    ok <- guess $ traceShow answer answer
    putStrLn $ if ok then "Success!" else "NG!"


-- | 部屋ラベルと部屋までの道のり(プラン)
type RoomP  = (RoomLabel, ParsedPlan)

-- | ドアは全部開いたが, まだ部屋番号(RoomIndex)が確定していない部屋データ
type RoomRaw = (RoomP, [(Door, RoomP)])

-- | 部屋番号が確定した部屋データ (まぁFはFixの略ということにしておこう(適当))
data RoomF = RoomF
  { rfLabel :: Int
  , rfDoors :: [(Door, RoomP)]
  , rfIndex :: RoomIndex
  , rfPlan  :: ParsedPlan
  } deriving Show

-- | 隣接する部屋の番号も確定した部屋データ.
-- なので RoomRaw→RoomF→RoomZの順で情報を確定させる
data RoomZ = RoomZ
  { rzIndex :: RoomIndex
  , rzLabel :: Int
  , rzDoors :: [((Door, RoomP), RoomIndex)]
  , rzPlan  :: ParsedPlan
  } deriving Show

-- | 部屋番号を確定する
rawToFixRoom :: RoomRaw -> RoomIndex -> RoomF
rawToFixRoom ((label, plan), doors) rid =
    RoomF 
    { rfIndex = rid
    , rfLabel = label
    , rfDoors = sort doors
    , rfPlan  = plan
    }

-- 隣接の部屋番号も確定する
fixToZRoom :: RoomF -> [(Door,RoomIndex)] -> RoomZ
fixToZRoom room doors =
    RoomZ
    { rzLabel = rfLabel room
    , rzIndex = rfIndex room
    , rzPlan  = rfPlan  room
    , rzDoors = zip (sort $ rfDoors room) (map snd $ sort doors)
    }


solve :: Int -> IO Layout
solve limit  = do
    -- とりあえず１回解く
    (_,fixRooms,zRooms) <- solve' limit [] [] (0, [])
    mapM_ print fixRooms
    mapM_ print zRooms
    when (null zRooms) $ fail "Failed(1)."
    let (_,_,conn) = formatAnswer zRooms [] --ダミー
    -- 長さや部屋数の道をみつけて, 書き換えて元に戻ってくるプランを作る
    -- XXX これはダメ. 最初の部屋も重複しているので真に元に戻ってるかどうか分からない
    let (f,b) = findPath (floor $ fromIntegral(length zRooms)*1.5) conn
    fb' <- withAlterLabel (f++b)
    -- ラベル書き換えの固定プランをプレフィックスとして２回目を解く
    (_,fixRooms2,zRooms2) <- solve' limit [] [] (0, fb')
    mapM_ print fixRooms2
    mapM_ print zRooms2
    when (null zRooms2) $ fail "Failed(2)."
    -- 元のラベルがよくわからんので再取得
    labels <- checkOrigLabel zRooms2
    return $ formatAnswer zRooms2 labels


solve' 
    :: Int      -- ^ 探索上限
    -> [RoomF]  -- ^ 既知の部屋
    -> [RoomZ]  -- ^ 確定した部屋
    -> RoomP    -- ^ いま調べる対象の部屋
    -> IO (RoomIndex, [RoomF], [RoomZ])  -- ^ 対象部屋の番号と更新された部屋情報
solve' 0       _       _       _       = error "limit reached, abort."
solve' limit  fixRooms zRooms (_,plan) = do
    putStrLn $ "count " ++ show limit ++ ", fixRooms=" ++ show(length fixRooms) ++ ", zRooms=" ++ show(length zRooms) ++ ", plan=" ++ renderPlan plan
    -- とりあえず全部のドアを開ける
    raw@(_room, neighbors) <- openAllDoor plan
    -- ドアパターンを既知の部屋を比較して部屋番号を確定する
    let (known, rid, fixRooms') = updateFixRooms fixRooms raw
    -- 既知の部屋(つまり別の枝ですでに探索中の部屋)だったらここで終わり
    if known
        then return (rid, fixRooms', zRooms)
        else do
        -- 隣接の部屋の番号も確定させる
        (nids, _limit', fixRooms'', zRooms') <-  foldrM f ([],limit-1,fixRooms',zRooms) (map snd neighbors)
        -- ようやく全部確定
        return (rid, fixRooms'', fixToZRoom (rawToFixRoom raw rid) (zip[0..]nids) : zRooms')
    where
        -- | solve' を foldrM するのにラップしてる版. accに隣接部屋番号を蓄積する
        f :: RoomP -> ([RoomIndex], Int, [RoomF], [RoomZ]) -> IO ([RoomIndex], Int, [RoomF], [RoomZ])
        f n (acc,lim,frs,zrs) = do
            (i,frs',zrs') <- solve' lim frs zrs n
            return (i:acc, lim-1, frs', zrs')


-- | 0部屋から始まる長さ n のプランをみつける, ついでに帰りのプランも返す
findPath :: Int -> [((RoomIndex, Door), (RoomIndex, Door))] -> (ParsedPlan, ParsedPlan)
findPath n conn = let (xs,ys) = unzip [(PassDoor d1, PassDoor d2) | (d1,d2) <-reverse $ head $ go n conn2 0 [] ] in (xs, reverse ys)
    where
        conn2 = groupBy((==)`on`fst.fst) $ nubBy((==)`on`fst) $ sort $ concat[ [(rd1,rd2),(rd2,rd1)] | (rd1,rd2)<-conn ]

        go :: Int -> [[((RoomIndex, Door), (RoomIndex, Door))]] -> RoomIndex -> [(RoomIndex,Door)] -> [[(Door,Door)]]
        go _ [cs] _ acc = [acc]
        go 0 _    _ acc = [acc]
        go k css r1 acc =
            let Just cs = find((==r1).fst.fst.head) css
            in [ (r2, (d1,d2):acc) | ((_,d1),(r2,d2)) <-cs, r1 /= r2 && (d1,d2)`notElem`acc ]
               >>= uncurry (go (k-1) css)


formatAnswer :: [RoomZ] -> [RoomLabel] -> Layout
formatAnswer rooms labels =
    ( labels
    , 0
    , calcDoorPair rooms
    )


-- | とりあえずペアになってるドアを貪欲につなげていくだけ.
-- 外に繋がるドアは, 等価な重複がなければ, 必ず逆向きがあるのでペアにして潰してく
calcDoorPair ::  [RoomZ] -> [((RoomIndex, Door), (RoomIndex, Door))]
calcDoorPair rooms =
    go [] [ ((room1,room2),d)  | r <-rooms, let room1 = rzIndex r, ((d,_),room2)<-rzDoors r]
    where
        go :: [((RoomIndex, Door), (RoomIndex, Door))] -> [((RoomIndex, RoomIndex), Door)] -> [((RoomIndex, Door), (RoomIndex, Door))]
        go acc []     = acc
        go acc (((r1,r2),d):rests) 
            | r1 == r2  = go (((r1,d),(r1,d)):acc) rests
            | otherwise = let (xs,((_,_),c):ys) = break (\((a,b),_)-> r1==b && r2==a) rests
                          in go (((r1,d),(r2,c)):acc) (xs++ys)


-- | planの各要素の前後にラベル書き換え操作を追加する (なので長さは +1 される)
withAlterLabel :: ParsedPlan -> IO ParsedPlan
withAlterLabel plan = do
    gen   <- Rand.createSystemRandom
    a     <- fmap  AlterLabel $ Rand.uniformR (0::RoomLabel, 3) gen
    alters<- fmap (map AlterLabel) $ replicateM (length plan) (Rand.uniformR (0::RoomLabel, 3) gen)
    return $ (a:) $ concat $ transpose [plan, alters]


-- | 固定プランプレフィックスからラベル書き換え操作を除外して, 元のラベルを確認する
checkOrigLabel :: [RoomZ] -> IO [RoomLabel]
checkOrigLabel zrooms = do
    (rss,_) <- explore $ map renderPlan [[PassDoor d| PassDoor d <-plan] | plan<- map rzPlan $ sortBy(compare`on`rzIndex) zrooms]
    return $ map last rss


allDoors :: ParsedPlan
allDoors = [ PassDoor i | i<-[0..5]]

-- | プランで辿りつく部屋の全部のドアを開けるだけ
openAllDoor :: ParsedPlan -> IO RoomRaw
openAllDoor plan = do
    (rss, _count) <- explore [ renderPlan(plan++[door]) | door <-allDoors]
    let len = length plan
        label = head $ drop len $ head rss
    return ((label,plan), [(i, (r, plan++[PassDoor i])) | (i,r)<- zip [0..] [ last rs | rs <-rss]] )


-- | 既知の部屋リストを更新つつ, 対象の部屋の番号を確定する
updateFixRooms :: [RoomF] -> RoomRaw -> (Bool, RoomIndex, [RoomF])
updateFixRooms fixs raw =
    case catMaybes (map (`isSameRoom`raw) fixs) of
        rid:_ -> (True, rid, fixs) -- 既知
        _     -> (False, nextIndex, rawToFixRoom raw nextIndex : fixs) -- 新規
    where
        -- 番号は新しい部屋が見つかった順に振っていくだけ
        nextIndex :: RoomIndex
        nextIndex = maximum (-1 : map rfIndex fixs) + 1


-- | 同じ部屋(==同じラベルでドアパターンも同じ)だったら部屋番号を返す
isSameRoom :: RoomF -> RoomRaw -> Maybe RoomIndex
isSameRoom r1 ((label2, _), doors2) =
    if label1 == label2 && doors1 == (sort $ map f doors2) then Just (rfIndex r1) else Nothing
    where
        label1 = rfLabel r1
        doors1 = map f (rfDoors r1) -- ソート済み

        f (a,(b,_)) = (a,b)
