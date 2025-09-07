{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ParallelListComp #-}

module LabelledBucket where

import Control.Monad
import Data.Function
import Data.List
import Data.String
import Data.Word
import Data.Array.IO
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map.Strict qualified as Map

import Base (Layout, RoomIndex)
import Base qualified
import ClientWrapper (Driver (..))

------------------------------------------------------------

{- $setup
>>> :seti -XOverloadedStrings
 -}

-- 部屋のラベル
newtype Label = L Word8 deriving (Eq, Ord, Enum, Ix, Num, Show)

instance IsString Label where
  fromString s = L (read s)

type Result = [Label]

-- ドア番号 0 - 5
newtype Door' = D Word8 deriving (Eq, Ord, Enum, Show)

allDoors :: [Door']
allDoors = map D [0..5]

newtype Plan' = P [Door'] deriving (Eq, Ord, Semigroup)

pstring :: Plan' -> String
pstring (P xs) = [c | D w <- xs, c <- show w]

instance Show Plan' where
  -- Door が 2桁以上のときは問題あり
  show plan = "P " ++ show (pstring plan)

instance IsString Plan' where
  -- Door が 2桁以上のときは問題あり
  fromString s = P [D (read [c]) | c <- s]

plength :: Plan' -> Int
plength (P ds) = length ds

-- 部分的な部屋の情報
--   ドアを開けた先のラベル
type PRoom = Set (Door', Label)

noOpenDoors :: PRoom
noOpenDoors = mempty

type PRooms = Map Plan' PRoom

-- ラベルごとの部屋のドアの情報
type CandBucket = IOArray Label PRooms

pprsPRooms :: PRooms -> [String]
pprsPRooms rs = [show path ++ " -> " ++ show pr | (path, pr) <- Map.toList rs]

noPlan :: Map Plan' PRoom
noPlan = mempty

newCandBucket :: IO CandBucket
newCandBucket = do
  newArray (L 0b00, L 0b11) noPlan

pprsCandBucket :: CandBucket -> IO [String]
pprsCandBucket bucket = do
  let pprs lb rs = (show lb ++ ":") : [ "  " ++ s | s <- pprsPRooms rs ]
  as <- getAssocs bucket
  pure [x | (lb, rs) <- as, x <- pprs lb rs]

withLabel
  :: (String -> IO ())
  -> CandBucket -> Label
  -> String
  -> (PRooms -> PRooms)
  -> IO ()
withLabel putLog bucket lb tag h = do
  rs0 <- readArray bucket lb
  let rs1 = h rs0
  putLog $ unlines $
    [ show lb ++ ": " ++ tag ++ ":" ]    ++
    [ "  " ++ s | s <- pprsPRooms rs0 ]  ++
    [ "    ==>> "]                       ++
    [ "  " ++ s | s <- pprsPRooms rs1 ]
  writeArray bucket lb rs1

{-
addPlan_ :: Plan' -> PRooms -> PRooms
addPlan_ path = Map.insert path noOpenDoors

addPlan
  :: (String -> IO ())
  -> CandBucket -> Label
  -> Plan' -> IO ()
addPlan putLog bucket lb path =
  withLabel putLog bucket lb "path" (addPlan_ path)
 -}

addDoor_
  :: Plan' -> Door' -> Label
  -> PRooms -> PRooms
addDoor_ path door doorL =
  Map.insertWith Set.union path (Set.singleton (door, doorL))

addDoor
  :: (String -> IO ())
  -> CandBucket -> Label
  -> Plan' -> Door' -> Label -> IO ()
addDoor putLog bucket lb path door doorL =
  withLabel putLog bucket lb "door" (addDoor_ path door doorL)

-----

runExploreIO
  :: (String -> IO ())
  -> CandBucket
  -> Plan' -> Result -> IO ()
runExploreIO _putLog _bucket  _plan     []  = fail "runExplore: Result list is null!"
runExploreIO  putLog  bucket (P ds) (r:rs)  = recExploreIO putLog bucket id r ds rs

recExploreIO
  :: (String -> IO ())
  -> CandBucket
  -> ([Door'] -> [Door']) -> Label
  -> [Door'] -> [Label] -> IO ()
recExploreIO  putLog  bucket = rec_
  where rec_ _  _      []     []     = pure ()
        rec_ ps roomL (d:ds) (r:rs)  = do
          step
          rec_ (ps . (d:)) r ds rs
          where step = withLabel putLog bucket roomL "recExplore.step" $
                  addDoor_ (P $ ps []) d r
        rec_ ps roomL  ds     rs     =
          fail $ "recExploreIO: inconsistent: " ++ info
          where info =
                  intercalate ", "
                  [ k ++ "=" ++ v
                  | (k, v) <- [ ("path", show (ps []))
                              , ("room", show roomL)
                              , ("plan", show ds)
                              , ("results", show rs)
                              ]
                  ]

-----

-- 見つかったラベル数が、部屋の全体数に達した場合、同じラベル内は同じ部屋
reduceUniqueLabels_ :: PRooms -> PRooms
reduceUniqueLabels_ prs = prs1
  where (plans, rs) = unzip $ Map.toList prs
        u = Set.unions rs
        prs1 = Map.fromList [(plan, u) | plan <- plans]

reduceUniqueLabels
  :: (String -> IO ())
  -> CandBucket -> IO ()
reduceUniqueLabels _putLog bucket = do
  as <- getAssocs bucket
  sequence_ [writeArray bucket lb (reduceUniqueLabels_ rs) | (lb, rs) <- as]

-- plans: "000","123","213","333"
-- results: [0,0,0,0],[0,1,0,1],[0,0,1,2],[0,1,2,0]

{-
* 全てのドアの先のラベルを確認した部屋のみを列挙
 -}
filledRooms_ :: PRooms -> PRooms
filledRooms_ = Map.filter ((== 6). Set.size)

filledRooms :: CandBucket -> IO [(Label, PRooms)]
filledRooms bucket = do
  as <- getAssocs bucket
  pure [(lb, filledRooms_ prs) | (lb, prs) <- as]

{-
* ドアの先のラベルのパターンで sort して group
 -}
byRooms_ :: PRooms -> [(PRoom, [Plan'])]
byRooms_ prs =
  map rplans $
  groupBy ((==) `on` snd) $
  sortOn snd $
  Map.toList prs
  where rplans ((pf, r):xs) = (r, pf : map fst xs)
        rplans  []          = error "groupBy result element, not reach"

type IndexedRoom = (RoomIndex, Label, (PRoom, [Plan']))

byRooms :: CandBucket -> IO [IndexedRoom]
byRooms bucket = do
  fillss <- filledRooms bucket
  pure [(ix, lb, room) | ix <- [0..] | (lb, fills) <- fillss, room <- byRooms_ fills ]

pprByRoom :: RoomIndex -> Label -> (PRoom, [Plan']) -> String
pprByRoom ix lb (pr, plans) =
  show ix <> ": " <> show lb <> ": " <> show pr <> " " <> show plans

-----

checkWithSize :: Int -> [a] -> IO ()
checkWithSize size rooms = do
  let roomCount = length rooms
  when (roomCount < size) $ fail $ "checkWithSize: " <> "roomCount=" <> show roomCount <> " < " <> "size=" ++ show size

plansPrefixNotSrc :: Int -> [IndexedRoom] -> IO [Plan']
plansPrefixNotSrc bdepth rooms = do
  when (not $ null nsrc) $
    putStr $ unlines $
    "not src rooms:" : ["  " ++ pprByRoom ix lb room | (ix, lb, room) <- nsrc]
  pure [p | (_ix, _lb, (_room, p@(P {}) : _)) <- nsrc]
  where
    srcs = filter ((< (bdepth - 1)) . plength)
    roomPlan (_ix, _lb, (_room, plan)) = plan
    nsrc = filter (null . srcs . roomPlan) rooms


-----

debug :: Bool
debug = False

type LayoutConn = ((RoomIndex, Base.Door), (RoomIndex, Base.Door))

getLayout :: [Plan'] -> [(RoomIndex, Label, (PRoom, [Plan']))] -> IO Layout
getLayout srcs rooms = do
  putStrLn ""
  putStr $ unlines [pprByRoom ix lb room | (ix, lb, room) <- rooms]

  putStr $ unlines $
    ("by-plan" :)
    ["  " <> show x | x <- Map.toList byPlan]
  putStr $ unlines $
    "oneways" :
    ["  " <> show o | o <- oneways]
  putStr $ unlines $
    "self connections:" :
    ["  " <> show s | s <- selfs]
  when debug $ do
    putStr $ unlines $
      "rlts:" :
      ["  " <> show s | s <- rlts]
    putStr $ unlines $
      "rgts:" :
      ["  " <> show s | s <- rgts]
  printCount "eqs" eqs
  printCount "lts" lts
  printCount "gts" gts
  putStr $ unlines $
    "mutial connections:" :
    ["  " <> show s | s <- mutuals]

  pure (roomsLb, startIx, selfs ++ mutuals)
  where roomsLb = [fromIntegral lb | (_, L lb, _)<- rooms]
        (startIx, _, _) = byPlan <!> P []

        selfs = [((si, door), (di, door)) | (si, di, door) <- eqs]
        mutuals =
          [ eqassert sl dg dl sg ((sl, doorl), (sg, doorg))
          | (sl, dl, doorl) <- lts
          | (dg, sg, doorg) <- gts
          ]
        eqassert sl dg dl sg x
          | sl == dg && dl == sg  = x
          | otherwise             = error $ "inconsistent: " ++ show (sl, dl) ++ " =/= " ++ show (sg, dg)

        printCount tag xs = putStrLn $ tag ++ ": " ++ show (length xs)
        eqs = [t | (t@(si, di, _door), _) <- oneways, si == di]
        rlts = [t | (t@(si, di, _door), _) <- oneways, si < di]
        lts = usort rlts
        rgts = [(di, si, door) | ((si, di, door), _) <- oneways, si > di]
        gts = usort rgts
        usort = Set.toList . Set.fromList
        oneways =
          [ ((si, di, fromIntegral dw), (src, dst))
          | src <- srcs
          , door@(D dw) <- allDoors
          , let dst = (src :: Plan') <> P [door]
                (si, _slb, _sroom) = byPlan <!> src
                (di, _dlb, _droom) = byPlan <!> dst
          ]

        byPlan = Map.fromList [(plan, (ix, lb, room)) | (ix, lb, (room, plans)) <- rooms, plan <- plans ]

        m <!> k = maybe (error $ "not found: " ++ show k) id $ Map.lookup k m

-----

type Problem = (String, Int)

probatio, primus, secundus, tertius
  :: Problem

probatio  = ("probatio"  ,   3)
primus    = ("primus"    ,   6)
secundus  = ("secundus"  ,  12)
tertius   = ("tertius"   ,  18)

-----

type ResultI = [Int]

solveBF :: Driver -> Problem -> String -> Int -> IO (Layout, Int)
solveBF Driver{..} (prob, size) ename bdepth = do
  let plansBF = map P $ replicateM bdepth allDoors
      fromResultsI rs = [map fromIntegral ri | ri <- rs]

  rinitClient
  _ <- rselect prob ename
  bucket <- newCandBucket

  (results, qc1) <- rexplore [pstring p | p <- plansBF]

  let run plan = runExploreIO (\_ -> pure ()) bucket plan
  zipWithM_ run plansBF (fromResultsI results)
  putStr . unlines =<< pprsCandBucket bucket

  rooms <- byRooms bucket
  checkWithSize size rooms

  aprefs <- plansPrefixNotSrc bdepth rooms
  let fillconn = do
        let plans2 = [pref <> P [d1, d2] | pref <- aprefs, d1 <- allDoors, d2 <- allDoors ]
        putStrLn $ "plans2: " ++ show plans2
        (results2, qc2) <- rexplore [pstring p | p <- plans2]
        zipWithM_ run plans2 (fromResultsI results2)
        roomsF <- byRooms bucket
        pure (roomsF, aprefs, qc2)
  (rooms2, asrcs, qc) <- if null aprefs
                     then pure (rooms, [], qc1)
                     else fillconn

  checkWithSize size rooms2
  let srcs = [P p | sz <- [0 .. bdepth - 2], p <- replicateM sz allDoors]
  (,) <$> getLayout (srcs ++ asrcs) rooms2 <*> pure qc

solveFromResult :: Int -> [String] -> Int -> [ResultI] -> IO Layout
solveFromResult _size plans bdepth resultsI = do
  bucket <- newCandBucket
  let run plan = runExploreIO (\_ -> pure ()) bucket (fromString plan)
      results = [map fromIntegral ri | ri <- resultsI]
  zipWithM_ run plans results
  putStr . unlines =<< pprsCandBucket bucket

  fillss <- byRooms bucket

  let srcs = [P p | sz <- [0 .. bdepth - 2], p <- replicateM sz allDoors]
  getLayout srcs fillss

-----

_example1 :: IO ()
_example1 = do
  bucket <- newCandBucket
  let run plans = runExploreIO putStr bucket (fromString plans)
  run "000" [0,0,0,0]
  run "123" [0,1,0,1]
  run "213" [0,0,1,2]
  run "333" [0,1,2,0]
  putStr . unlines =<< pprsCandBucket bucket

-- "231025153214112435435242513423043543011400411314240201"
-- [0,2,0,2,2,0,2,0,2,0,2,0,1,1,1,1,0,2,0,1,1,0,2,2,0,2,0,2,2,0,2,2,2,0,2,2,0,1,1,1,0,1,1,0,2,0,2,0,1,1,0,1,1,1,1]

_example2 :: IO ()
_example2 = do
  bucket <- newCandBucket
  let run plans = runExploreIO putStr bucket (fromString plans)
  run
    "231025153214112435435242513423043543011400411314240201"
    [0,2,0,2,2,0,2,0,2,0,2,0,1,1,1,1,0,2,0,1,1,0,2,2,0,2,0,2,2,0,2,2,2,0,2,2,0,1,1,1,0,1,1,0,2,0,2,0,1,1,0,1,1,1,1]
  putStr . unlines =<< pprsCandBucket bucket
  reduceUniqueLabels putStr bucket
  putStr . unlines =<< pprsCandBucket bucket

------------------------------------------------------------

{-
type RoomId = Int

data Room =
  Room
  { id_      :: RoomId
  , label_   :: Label
  , doors_   :: IORef (Set (Door, Label))
  }

type Rooms = IOArray Int Room
 -}
