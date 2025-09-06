module Main where

import Data.List
import Debug.Trace
import Data.Tuple (swap)
import System.Environment

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


type Room = Int

type RoomId = Int
type Path   = String
type RoomP  = (Room, Path)

type RoomRaw = (RoomP, [(Door, RoomP)])

data RoomF = RoomF
  { rfLabel :: Int
  , rfDoors :: [(Door, RoomId)]
  , rfId    :: RoomId
  , rfPath  :: Path
  } deriving Show

fromRoomRawToF :: RoomRaw -> RoomF
fromRoomRawToF ((rno, path), ds) =
    RoomF 
    { rfLabel = rno
    , rfId = rno
    , rfPath = path
    , rfDoors = [(d,r) | (d,(r,ps))<-ds]
    }

solve :: Int -> IO [RoomF]
solve limit =  solve' limit [] [(0,"")]



solve' :: Int -> [RoomF] -> [RoomP] -> IO [RoomF]
solve' 0 knownRooms _  = trace "limit"   $ return []
solve' _ knownRooms [] = trace "ended" $ return knownRooms
solve' limit knownRooms nrs@((_rno,path):nextRooms) = do
    traceIO $ "count " ++ show limit ++ ", knowns=" ++ show(length knownRooms) ++ ", nexts=" ++ show(length nextRooms+1)
    mapM_ print knownRooms
    print nrs
    raw <- openAllDoor path
    let (knowns', nexts) = updateKnownRooms knownRooms raw
        nexts'           = foldr (flip mergeRooms) nextRooms nexts
    solve' (limit-1) knowns' nexts'

formatAnswer :: [RoomF] ->  ([Room], Room, [((Room, Door), (Room, Door))])
formatAnswer rooms =
    (sort $ map rfLabel rooms, 0, calcDoorPair rooms)


calcDoorPair ::  [RoomF] -> [((Room, Door), (Room, Door))]
calcDoorPair rooms = trace (show conns) $
    pair [] conns
    where
        conns = [ ((room1,room2),d)  | r <-rooms, let room1 = rfLabel r, (d,room2)<-rfDoors r]

        pair acc []     = acc
        pair acc (((r1,r2),d):rests) 
            | r1 == r2  = pair (((r1,d),(r1,d)):acc) rests
            | otherwise = let (xs,((a,b),c):ys) = break (\((a,b),c)-> r1==b && r2==a) rests
                          in pair (((r1,d),(r2,c)):acc) (xs++ys)


allDoors :: [Path]
allDoors = ["0","1","2","3","4","5"]

openAllDoor :: Path -> IO RoomRaw
openAllDoor path = do
    (rss, _count) <- explore (map (path++) allDoors)
    let len = length path
        --部屋番号
        rno = head $ drop len $ head rss
    return ((rno,path), [(i, (r, path++show i)) | (i,r)<- zip [0..] [ last rs | rs <-rss]] )


--既知の部屋リストを更新して, 次に探索すべき部屋を返す
updateKnownRooms :: [RoomF] -> RoomRaw -> ([RoomF], [RoomP])
updateKnownRooms knowns raw =
    if any (isSameLabelAndDoors roomf) knowns
        then (knowns, [])
        else (roomf:knowns, map snd (snd raw))
        -- TODO この辺でラベル重複をみつけて区別する
    where
        roomf = fromRoomRawToF raw
    
mergeRooms :: [RoomP] -> RoomP -> [RoomP]
mergeRooms rs r =
    if (fst r)`elem` (map fst rs)
        then rs else rs++[r]

isSameLabelAndDoors :: RoomF -> RoomF -> Bool
isSameLabelAndDoors r1 r2 =
    rfLabel r1 == rfLabel r2 && rfDoors r1 == rfDoors r2
-- TODO 同じラベルで違いドアパターンだった場合, 部屋を区別するためにrfIdに別数字を割り当てる