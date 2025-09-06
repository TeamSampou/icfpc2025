{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Searching where

import           Data.Aeson (ToJSON(..), (.=), object, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char (intToDigit)
import           Data.List (nub, minimumBy)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           System.Environment (getArgs)

import           Base (Layout)
import           Client (initClient, select, explore, guess)

--------------------------------------------------------------------------------
-- 型定義
--------------------------------------------------------------------------------

type Sym    = Int                     -- 扉: 0..5
type Word6  = [Sym]                   -- ルートプラン（語）
type Label2 = Int                     -- 2bit ラベル: 0..3

-- 観測テーブル（L* 風: P, S, 出力 out(x ++ s)）
data ObsTable = ObsTable
  { sigma :: [Sym]                    -- {0..5}
  , pSet  :: Set Word6                -- 接頭辞集合（prefix-closed）
  , sSet  :: Set Word6                -- 接尾辞集合（suffix の集まり）
  , outV  :: Map Word6 Label2         -- 観測値 out(w) = w の到達状態のラベル
  }
  deriving (Show)

-- Moore オートマトン（仮説）
-- 状態は P の代表語を番号付けしたものとみなす
data Automaton = Automaton
  { states      :: [Word6]            -- 各状態の代表語（アクセス語）
  , stateIndex  :: Map Word6 Int      -- 代表語→stateId
  , lambdaOut   :: Map Int Label2     -- stateId → λ
  , delta       :: Map (Int, Sym) Int -- (stateId, a) → stateId
  , alphabet    :: [Sym]
  } deriving (Show)

--------------------------------------------------------------------------------
-- ユーティリティ
--------------------------------------------------------------------------------

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _      = True
isPrefixOf _  []     = False
isPrefixOf (x:xs) (y:ys) = x==y && isPrefixOf xs ys

-- 語の連結
(⊕) :: Word6 -> Word6 -> Word6
(⊕) = (++)

-- 接頭辞閉包（εを含む）
prefixClosure :: Set Word6 -> Set Word6
prefixClosure = S.fromList . concatMap prefixes . S.toList
  where
    prefixes w = [ take k w | k <- [0..length w] ]

-- prefix-closed 圧縮：集合 R をその「接頭辞最大要素」の集合へ
-- （これらを問い合わせれば R の全接頭辞 out が一括で得られる）
compressPrefixClosed :: Set Word6 -> [Word6]
compressPrefixClosed r =
  let ws = S.toList r
  in filter (\w -> not (any (\v -> w /= v && isPrefixOf w v) ws)) ws

-- 行ベクトル row(x) = [ out(x⊕s) | s∈S ]（S の列順は Set のリスト化順）
rowOf :: ObsTable -> Word6 -> [Label2]
rowOf t x = [ outV t M.! (x ⊕ s) | s <- S.toList (sSet t) ]

-- P ∪ P·Σ
pUnionPSigma :: ObsTable -> Set Word6
pUnionPSigma t =
  let p  = pSet t
      ps = S.fromList [ x ++ [a] | x <- S.toList p, a <- sigma t ]
  in S.union p ps

--------------------------------------------------------------------------------
-- 観測テーブル：必要語・問い合わせバッチ
--------------------------------------------------------------------------------

-- この表を満たすために必要な語集合（完全版）
--   R = { x⊕s | x ∈ (P ∪ PΣ), s ∈ S }
requiredWords :: ObsTable -> Set Word6
requiredWords t =
  let xs = S.toList (pUnionPSigma t)
      ss = S.toList (sSet t)
  in S.fromList [ x ⊕ s | x <- xs, s <- ss ]

-- まだ観測が無い語（不足分）
missingWordsSet :: ObsTable -> Set Word6
missingWordsSet t =
  let need = requiredWords t
      have = S.fromList (M.keys (outV t))
  in need `S.difference` have

-- 「不足分のみ」を prefix-closed 圧縮して問い合わせるプラン群
batchPlansMissing :: ObsTable -> [Word6]
batchPlansMissing = compressPrefixClosed . missingWordsSet

-- 観測 ingestion: /explore の結果から (w → out(w)) を流し込む
_ingestObservations :: ObsTable -> Map Word6 Label2 -> ObsTable
_ingestObservations t newOut = t { outV = M.union newOut (outV t) }
-- 上書きせず既知を優先（不整合時の保険）。必要なら反転する:
ingestObservations :: ObsTable -> Map Word6 Label2 -> ObsTable
ingestObservations t newOut = t { outV = M.union (outV t) newOut }
  
--------------------------------------------------------------------------------
-- 観測テーブルの更新（閉包・整合性チェック）
--------------------------------------------------------------------------------

-- 閉包チェック：∀x∈P, a∈Σ, ∃y∈P s.t. row(xa)=row(y)
-- 破れていれば、P に xa を追加（prefix 閉包で拡張）
ensureClosed :: ObsTable -> ObsTable
ensureClosed t =
  let p      = pSet t
      rowsP  = M.fromList [ (x, rowOf t x) | x <- S.toList p ]
      need   =
        [ xa
        | x <- S.toList p
        , a <- sigma t
        , let xa = x ++ [a]
        , let r  = rowOf t xa
        , not (any (\(_,r') -> r'==r) (M.toList rowsP))
        ]
  in if null need
       then t
       else t { pSet = prefixClosure (S.union p (S.fromList need)) }

-- 整合性チェック（L* の consistency）：
--   row(x) = row(y) なのに、ある a で row(xa) ≠ row(ya) なら
--   S に a·s を追加（ここで s は row(xa) と row(ya) を区別する既存の列）。
ensureConsistent :: ObsTable -> ObsTable
ensureConsistent t =
  let pList = S.toList (pSet t)
      sList = S.toList (sSet t)
      eqPairs =
        [ (x,y)
        | (i,x) <- zip [0..] pList
        , y <- drop (i+1) pList
        , rowOf t x == rowOf t y
        ]
      toAdd =
        [ a : s
        | (x,y) <- eqPairs
        , a <- sigma t
        , let rxa = rowOf t (x ++ [a])
        , let rya = rowOf t (y ++ [a])
        , rxa /= rya
        , s <- sList
        , (outV t M.! ((x ++ [a]) ++ s)) /= (outV t M.! ((y ++ [a]) ++ s))
          -- ↑ rxa /= rya なので、必ず差が出る列 s が少なくとも1つ存在。
          -- そのうち最初のもの（sList順）を採用。ここは全列でもOK。
        , let _ = ()  -- for list comp scoping
        ]
      -- 重複を削り、最小限だけ追加（1衝突につき最初の s だけ）
      toAddMin :: [Word6]
      toAddMin =
        nub $ pickFirstPer (pairsSig toAdd)
      -- (a:s) を (x,y,a) のキーで最初の1つに絞るための補助
      -- ここでは (a,s) 自体をキーに使い、単純に重複除去
      pairsSig = id
      pickFirstPer = id
  in if null toAdd
       then t
       else t { sSet = S.union (sSet t) (S.fromList toAddMin) }

-- 注意：
-- * ensureClosed / ensureConsistent は、必要な観測が outV に揃っている前提で使う。
--   「不足分問い合わせ → ingest → ensureClosed → ensureConsisten （何か増えたら）不足分問い合わせ …」を繰り返す。

--------------------------------------------------------------------------------
-- 観測テーブル初期化
--------------------------------------------------------------------------------

initTable :: ObsTable
initTable = ObsTable
  { sigma = [0..5]
  , pSet  = S.singleton []      -- P={ε}
  , sSet  = S.singleton []      -- S={ε}
  , outV  = M.empty
  }

--------------------------------------------------------------------------------
-- 仮説オートマトンの構築
--------------------------------------------------------------------------------

-- P の行で同値類を作り、代表を chosenRep に
equivClasses :: ObsTable -> [[Word6]]
equivClasses t =
  let p = S.toList (pSet t)
      groups = foldr insertG [] p
      insertG x [] = [[x]]
      insertG x (g:gs)
        | rowOf t x == rowOf t (head g) = (x:g):gs
        | otherwise                     = g : insertG x gs
  in groups

chosenRep :: [Word6] -> Word6
chosenRep = minimumBy (comparing (\w -> (length w, w))) -- 短い→辞書順

buildAutomaton :: ObsTable -> Automaton
buildAutomaton t =
  let classes = equivClasses t
      reps    = map chosenRep classes
      idx     = M.fromList (zip reps [0..])
      labOf w = outV t M.! w
      lam     = M.fromList [ (idx M.! r, labOf r) | r <- reps ]

      -- row 同値な P 内代表を返す（閉包が成り立っていれば必ず見つかる）
      repFor :: Word6 -> Word6
      repFor w =
        let Just r = findRep w
        in r
      findRep w =
        let target = rowOf t w
        in case [ r | cls <- classes
                    , let r = head cls
                    , rowOf t r == target ] of
             (r:_) -> Just (chosenRep [r])  -- 代表正規化
             []    -> Nothing

      del     = M.fromList
                  [ ((idx M.! r, a), idx M.! repFor (r ++ [a]))
                  | r <- reps
                  , a <- sigma t
                  ]
  in Automaton
      { states     = reps
      , stateIndex = idx
      , lambdaOut  = lam
      , delta      = del
      , alphabet   = sigma t
      }

--------------------------------------------------------------------------------
-- W 法（characterizing set W を仮説から構成）
--------------------------------------------------------------------------------

-- δ*(q, w)
deltaStar :: Automaton -> Int -> Word6 -> Int
deltaStar aut q []     = q
deltaStar aut q (a:as) =
  case M.lookup (q,a) (delta aut) of
    Just q' -> deltaStar aut q' as
    Nothing -> q

-- λ(δ*(q, w))
outAt :: Automaton -> Int -> Word6 -> Label2
outAt aut q w =
  let q' = deltaStar aut q w
  in lambdaOut aut M.! q'

-- 2状態を区別する最短接尾辞を BFS で探索
distinguishingSuffix :: Automaton -> Int -> Int -> Maybe Word6
distinguishingSuffix aut p q
  | lambdaOut aut M.! p /= lambdaOut aut M.! q = Just []
  | otherwise = bfs (S.singleton []) [[]]
  where
    alph = alphabet aut
    bfs _    [] = Nothing
    bfs seen (w:ws) =
      let owP = outAt aut p w
          owQ = outAt aut q w
      in if owP /= owQ
           then Just w
           else
             let nexts  = [ w ++ [a] | a <- alph ]
                 unseen = filter (`S.notMember` seen) nexts
             in bfs (foldr S.insert seen unseen) (ws ++ unseen)

-- W（characterizing set）
computeW :: Automaton -> Set Word6
computeW aut =
  let qs    = [0 .. length (states aut) - 1]
      pairs = [ (i,j) | (k,i) <- zip [0..] qs, j <- drop (k+1) qs ]
      ws    = [ w | (p,q) <- pairs
                  , Just w <- [distinguishingSuffix aut p q] ]
  in S.fromList (if null ws then [[]] else ws)

-- W 法テスト生成: P · Σ^≤k · W
genWTests :: Int -> Automaton -> [Word6]
genWTests k aut =
  let p = states aut
      w = S.toList (computeW aut)
      mids = sigmaPowers (alphabet aut) k
  in nub [ pfx ++ mid ++ suf | pfx <- p, mid <- mids, suf <- w ]

-- Σ^≤k
sigmaPowers :: [Sym] -> Int -> [Word6]
sigmaPowers alph k = concat [ sequencesOfLen n alph | n <- [0..k] ]
  where
    sequencesOfLen 0 _    = [[]]
    sequencesOfLen n alph = [ a:rest | a <- alph, rest <- sequencesOfLen (n-1) alph ]

--------------------------------------------------------------------------------
-- 提出用 地図 JSON の構築（to.door 割付を整合的に行う）
--------------------------------------------------------------------------------

-- 既存の Automaton を前提に
buildLayout :: Automaton -> Layout
buildLayout aut =
  let n       = length (states aut)
      labels  = [ lambdaOut aut M.! q | q <- [0..n-1] ]
      start   = stateIndex aut M.! []  -- 空語クラスの代表＝開始部屋

      -- 各部屋ごとに「既に使ったドア」を管理して、重複なく接続を作る
      initUsed = M.fromList [ (q, S.empty) | q <- [0..n-1] ]

      (edges, _) = foldl addForState ([], initUsed) [0..n-1]

      addForState (acc, used) q =
        foldl (addForDoor q) (acc, used) (alphabet aut)

      addForDoor q (acc, used) a =
        if S.member a (used M.! q)
          then (acc, used)  -- もう結線済み
          else
            let q'     = delta aut M.! (q,a)
                usedQ  = used M.! q
                usedQ' = used M.! q'

                -- 相手側で未使用かつ δ(q',b)=q の b を優先
                cand = [ b | b <- alphabet aut
                           , M.lookup (q',b) (delta aut) == Just q
                           , not (S.member b usedQ') ]
                b = case cand of
                      (b0:_) -> b0
                      []     -> head [ b0 | b0 <- alphabet aut
                                          , not (S.member b0 usedQ') ]

                edge  = ((q,a),(q',b))
                used' = M.insert q  (S.insert a usedQ) $
                        M.insert q' (S.insert b usedQ') used
            in (edge:acc, used')

  in (labels, start, reverse edges)

--------------------------------------------------------------------------------
-- /explore の I/O を ObsTable に取り込むための補助
--------------------------------------------------------------------------------

-- plan（[Int]）→ サーバ送信用の String
-- 0..5 が対象なので intToDigit で安全に1文字化
encodePlan :: Word6 -> String
encodePlan = map intToDigit

-- /explore の返り（逐次ラベル列）から
-- すべての「接頭辞語」への out マップを作る
-- 例: plan=[0,2], results=[1,2,0] なら
--   []->1, [0]->2, [0,2]->0 を埋める
buildOutMapFromExplore :: [Word6] -> [[Label2]] -> M.Map Word6 Label2
buildOutMapFromExplore plans results =
  let prefixes w = [ take k w | k <- [0..length w] ]
      pairs = concat $ zipWith (\w outs -> zip (prefixes w) outs) plans results
  in  M.fromListWith (\old _new -> old) pairs


-- 不足語を「無くなるまで」埋める（/explore を必要回だけ回す）
fillAllMissing :: ( [String] -> IO ([[Int]], Int) )
               -> ObsTable
               -> IO ObsTable
fillAllMissing doExplore = go
  where
    go t =
      case batchPlansMissing t of
        []      -> pure t
        plans   -> do
          let payload = map encodePlan plans
          (results, _qc) <- doExplore payload
          let outMap = buildOutMapFromExplore plans results
          go (ingestObservations t outMap)

-- 安定化ループ：P/S を増やしたら必ず fillAllMissing を挟む
stabilizeTable :: ( [String] -> IO ([[Int]], Int) ) -> ObsTable -> IO ObsTable
stabilizeTable doExplore = loop
  where
    loop t0 = do
      -- 1) まず現状の P/S に対する不足を完全に埋める
      t1 <- fillAllMissing doExplore t0

      -- 2) 閉包チェック。P が増えたら不足が出るので、埋めてから再ループ
      let t2 = ensureClosed t1
      if pSet t2 /= pSet t1
        then loop t2
        else do
          -- 3) 整合性チェック。S が増えたら不足が出るので、埋めてから再ループ
          let t3 = ensureConsistent t2
          if sSet t3 /= sSet t2
            then loop t3
            else do
              -- 4) 念のため、整合性で S が不変でも不足が出ていないかを最終確認
              t4 <- fillAllMissing doExplore t3
              if S.null (missingWordsSet t4)
                   then pure t4
                   else loop t4

-- 追加の検証（W 法）を 1 回走らせて、未知のテスト語があれば回収する
verifyOnce :: ( [String] -> IO ([[Int]], Int) )
           -> ObsTable
           -> IO ObsTable
verifyOnce doExplore t = do
  let aut   = buildAutomaton t
  let tests = genWTests 1 aut
  let unknown = S.fromList tests `S.difference` M.keysSet (outV t)
  if S.null unknown
     then pure t
     else do
       -- テスト語も prefix-closed 圧縮してまとめ打ち
       let plans = compressPrefixClosed unknown
       let payload = map encodePlan plans
       (results, _qc) <- doExplore payload
       let outMap = buildOutMapFromExplore plans results
       -- 新しく入った観測を取り込んだ上で、再度安定化
       stabilizeTable doExplore (ingestObservations t outMap)



--------------------------------------------------------------------------------
-- 上位 API：地図の学習と提出 JSON の生成
--------------------------------------------------------------------------------

-- 1回の学習・検証・提出物生成をまとめて
learnAndGuess :: String -> IO Bool
learnAndGuess prob = do
  -- クライアント初期化
  initClient
  -- 問題選択
  select prob

  -- 初期化
  let t0 = initTable
  putStrLn " Initialized"

  -- 観測テーブルを安定化
  tStable <- stabilizeTable explore t0
  putStrLn $ "tStable : " ++ show tStable

  -- 追加の W 法検証（1回）
  tVerified <- verifyOnce explore tStable
  putStrLn $ "tVerified : " ++ show tVerified

  -- 仮説→提出 JSON
  let aut  = buildAutomaton tVerified
  putStrLn $ "learned automaton: " ++ show aut
  let layout = buildLayout aut
  putStrLn $ "layout: " ++ show layout
  
  -- 提出
  guess layout

-- | :main probatio
main :: IO ()
main = do
  prob:_ <- getArgs
  putStrLn $ "Problem: " ++ prob
  res <- learnAndGuess prob
  if res
    then putStrLn "Success!"
    else putStrLn "Failed."
