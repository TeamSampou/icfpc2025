module Main where

import Control.Monad (when)
import Data.List
import Data.Foldable (foldrM)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Debug.Trace
import Data.Tuple (swap)
import System.Environment

import Base
import Client


main :: IO ()
main =  do
    progName          <- getProgName
    problem:limit:_   <- getArgs
    initClient
    select problem
    rooms <- solve (read limit)
    if null rooms
        then fail "Failed."
        else do
            ok <- guess $ traceShow (formatAnswer rooms) (formatAnswer rooms)
            putStrLn $ if ok then "Success!" else "NG!"


-- | 部屋ラベルと部屋までの道のり(プラン)
type RoomP  = (RoomLabel, Plan)

-- | ドアは全部開いたが, まだ部屋番号(RoomIndex)が確定していない部屋データ
type RoomRaw = (RoomP, [(Door, RoomP)])

-- | 部屋番号が確定した部屋データ (まぁFはFixの略ということにしておこう(適当))
data RoomF = RoomF
  { rfLabel :: Int
  , rfDoors :: [(Door, RoomP)]
  , rfIndex :: RoomIndex
  , rfPlan  :: Plan
  } deriving Show

data RoomZ = RoomZ
  { rzLabel :: Int
  , rzDoors :: [((Door, RoomP), RoomIndex)]
  , rzIndex :: RoomIndex
  , rzPlan  :: Plan
  } deriving Show

-- | 部屋番号を確定する
rawToFixRoom :: RoomRaw -> RoomIndex -> RoomF
rawToFixRoom ((label, plan), doors) rid =
    RoomF 
    { rfLabel = label
    , rfIndex = rid
    , rfPlan  = plan
    , rfDoors = sort doors
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


solve :: Int -> IO [RoomZ]
solve limit = do
    (_,fixRooms,zRooms) <- solve' limit [] [] (0,"")
    putStrLn $ "fixRooms=" ++ show(length fixRooms) ++ ", zRooms=" ++ show(length zRooms)
    mapM_ print fixRooms
    mapM_ print zRooms
    return zRooms


solve' 
    :: Int      -- ^ 探索上限
    -> [RoomF]  -- ^ 既知の部屋
    -> [RoomZ]  -- ^ 確定した部屋
    -> RoomP    -- ^ いま調べる対象の部屋
    -> IO (RoomIndex, [RoomF], [RoomZ])  -- ^ 対象部屋の番号と更新された部屋情報
solve' 0       _       _       _       = error "limit reached, abort."
solve' limit  fixRooms zRooms (_,plan) = do
    putStrLn $ "count " ++ show limit ++ ", fixRooms=" ++ show(length fixRooms) ++ ", zRooms=" ++ show(length zRooms) ++ ", plan=" ++ plan
    when (limit`mod`5==1) $ mapM_ print zRooms
    -- とりあえず全部のドアを開ける
    raw@(room,neighbors) <- openAllDoor plan
    -- ドアパターンを既知の部屋を比較して部屋番号を確定する
    let (known, rid, fixRooms') = updateFixRooms fixRooms raw
    if known
        then return (rid, fixRooms', zRooms)
        else do
        -- 隣接の部屋の番号も確定させる
        (nids, limit', fixRooms'', zRooms') <-  foldrM f ([],limit-1,fixRooms',zRooms) (map snd neighbors)
        -- ようやく全部確定
        return (rid, fixRooms'', fixToZRoom (rawToFixRoom raw rid) (zip[0..]nids) : zRooms')
    where
        f n (rs,lim,frs,zrs) = do
            (i,frs',zrs') <- solve' lim frs zrs n
            return (rs++[i], lim-1, frs', zrs')


formatAnswer :: [RoomZ] -> Layout
formatAnswer rooms =
    ( map rzLabel $ sortBy (compare`on`rzIndex) rooms
    , 0
    , calcDoorPair rooms
    )


-- | とりあえずペアになってるドアを貪欲につなげていくだけ
calcDoorPair ::  [RoomZ] -> [((RoomIndex, Door), (RoomIndex, Door))]
calcDoorPair rooms =
    go [] [ ((room1,room2),d)  | r <-rooms, let room1 = rzIndex r, ((d,_),room2)<-rzDoors r]
    where
        go acc []     = acc
        go acc (((r1,r2),d):rests) 
            | r1 == r2  = go (((r1,d),(r1,d)):acc) rests
            | otherwise = let (xs,((a,b),c):ys) = break (\((a,b),c)-> r1==b && r2==a) rests
                          in go (((r1,d),(r2,c)):acc) (xs++ys)


allDoors :: [Plan]
allDoors = ["0","1","2","3","4","5"]

-- | プランで辿りつく部屋の全部のドアを開けるだけ
openAllDoor :: Plan -> IO RoomRaw
openAllDoor plan = do
    (rss, _count) <- explore (map (plan++) allDoors)
    let len = length plan
        label = head $ drop len $ head rss
    return ((label,plan), [(i, (r, plan++show i)) | (i,r)<- zip [0..] [ last rs | rs <-rss]] )


-- | 既知の部屋リストを更新つつ, 対象の部屋の番号を確定する
updateFixRooms :: [RoomF] -> RoomRaw -> (Bool, RoomIndex, [RoomF])
updateFixRooms fixs raw =
    case catMaybes (map (`isSameRoom`raw) fixs) of
        rid:_ -> (True, rid, fixs) -- 既知
        _     -> (False, nextIndex, rawToFixRoom raw nextIndex : fixs) -- 新規
    where
        -- 番号は新しい部屋が見つかった順に振っていくだけ
        nextIndex = maximum (-1 : map rfIndex fixs) + 1


-- | 同じ部屋(==同じラベルでドアパターンも同じ)だったら部屋番号を返す
isSameRoom :: RoomF -> RoomRaw -> Maybe RoomIndex
isSameRoom r1 ((label2, _), doors2) =
    traceShow (r1,label2,doors2,doors1 == (sort $ map f doors2)) $ 
    if label1 == label2 && doors1 == (sort $ map f doors2) then Just (rfIndex r1) else Nothing
    where
        label1 = rfLabel r1
        doors1 = map f (rfDoors r1) -- ソート済み

        f (a,(b,_)) = (a,b)
