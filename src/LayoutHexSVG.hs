{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module LayoutHexSVG
  ( layoutToSVGFile
  , guessToSVGFile
  ) where

import Data.Aeson (eitherDecode)
import Data.List (intercalate, foldl')
import Numeric   (showFFloat)
import qualified Data.ByteString.Lazy as BL
import System.Exit (die)

import Base (Layout, RoomLabel, RoomIndex, Door)
import TypesJSON

-- 2D ベクトル
type V2 = (Double, Double)
(+^) :: V2 -> V2 -> V2; (x1,y1) +^ (x2,y2) = (x1+x2, y1+y2)
(-^) :: V2 -> V2 -> V2; (x1,y1) -^ (x2,y2) = (x1-x2, y1-y2)
(*^) :: Double -> V2 -> V2; a *^ (x,y) = (a*x, a*y)
dot  :: V2 -> V2 -> Double; dot (x1,y1) (x2,y2) = x1*x2 + y1*y2
norm :: V2 -> Double; norm v = sqrt (max 1e-12 (dot v v))
unit :: V2 -> V2; unit v = let n = norm v in (fst v / n, snd v / n)
perp :: V2 -> V2; perp (x,y) = (-y, x)

fmt2 :: Double -> String
fmt2 x = showFFloat (Just 2) x ""
pt :: V2 -> String
pt (x,y) = fmt2 x ++ " " ++ fmt2 y

--------------------------------------------------------------------------------
-- 力学レイアウト
--------------------------------------------------------------------------------

data ForceParams = ForceParams
  { steps   :: Int, temp0 :: Double, cool :: Double
  , kRep    :: Double, kSpring :: Double, gravity :: Double
  , minSep  :: Double
  }

initialOnCircle :: Int -> Double -> [V2]
initialOnCircle n radius =
  [ (radius * cos t, radius * sin t)
  | i <- [0..n-1]
  , let t = 2*pi*fromIntegral i / fromIntegral (max 1 n) ]

step :: ForceParams -> [(Int,Int)] -> [V2] -> [V2]
step p es ps =
  let n   = length ps
      ctr = let (sx,sy) = foldl' (+^) (0,0) ps
                m = fromIntegral (max 1 n)
            in (sx/m, sy/m)
      fRep i =
        foldl' (+^) (0,0)
          [ let d    = pi -^ pj
                dist = norm d
                rep  = (kRep p / (dist*dist)) *^ d
                sep  = if dist < minSep p
                         then ((minSep p - dist) * 2.0) *^ unit d else (0,0)
            in rep +^ sep
          | j <- [0..n-1], j /= i
          , let pi = ps !! i, let pj = ps !! j ]
      fSpring i =
        foldl' (+^) (0,0)
          [ let d    = pj -^ pi
                dist = norm d
                rest = 1.10 * minSep p
                pull = (kSpring p * (dist - rest)) *^ unit d
            in pull
          | (u,v) <- es, i == u || i == v
          , let pi = ps !! i
          , let pj = ps !! (if i == u then v else u) ]
      fGrav i = let pi = ps !! i; dir = ctr -^ pi in gravity p *^ dir
      t       = temp0 p
      force   = [ fRep i +^ fSpring i +^ fGrav i | i <- [0..n-1] ]
      move    = [ let m = norm f in if m > t then (t/m) *^ f else f | f <- force ]
  in zipWith (+^) ps move

-- ---------------------------------------------------------------
--  線の張力で部屋位置をちょい動かす後処理（安全な小ステップ）
--    rad    : 六角の半径
--    iters  : 反復回数（例: 60〜150）
--    stepSz : 1回あたりの最大移動量（例: 0.15〜0.35）
-- ---------------------------------------------------------------
-- ---------------------------------------------------------------
--  線の張力で部屋位置をちょい動かす後処理（安全な小ステップ）
-- ---------------------------------------------------------------
relaxByEdges
  :: Double          -- ^ rad
  -> Int             -- ^ iters
  -> Double          -- ^ stepSz
  -> [((Int,Door),(Int,Door))]
  -> [V2]
  -> [V2]
relaxByEdges rad iters stepSz edges0 pos0 = go iters pos0
  where
    n        = length pos0
    minSep'  = 2*rad + 0.3*rad  -- 最小分離距離（安全マージン込み）
    kTension = 0.6              -- 線の張力係数（大きいほど縮む）
    kRepE    = 1.2              -- 近距離反発係数（大きいほど離れる）
    kGrav    = 0.0              -- 中心寄せ係数（大きいほど中央に寄る）

    clamp v =
      let m = norm v
      in if m <= stepSz || m <= 1e-12 then v else (stepSz/m) *^ v

    go 0 ps = ps
    go k ps =
      let -- 1) 線の張力（ドア位置を縮める）。各エッジ→2要素のリスト→concatで平坦化
          tensionForces = concat
            [ let p1 = doorPoint rad ci di
                  p2 = doorPoint rad cj dj
                  f  = kTension *^ (p2 -^ p1)
              in [(ri, f) , (rj, (-1) *^ f)]
            | ((ri,di),(rj,dj)) <- edges0
            , let ci = ps !! ri
            , let cj = ps !! rj
            ]

          -- 2) 近距離の反発（最小分離を死守）
          repulsions = concat
            [ let d    = (ps !! i) -^ (ps !! j)
                  dist = norm d
                  need = minSep' - dist
                  f    = if need > 0 then (kRepE * need) *^ unit d else (0,0)
              in [(i, f), (j, (-1) *^ f)]
            | i <- [0..n-1], j <- [i+1..n-1]
            ]

          -- 3) ごく弱い重力で中心に寄せる
          ctr   = let (sx,sy) = foldl' (+^) (0,0) ps
                      m = fromIntegral (max 1 n)
                  in (sx/m, sy/m)
          gravs = [ (i, kGrav *^ (ctr -^ (ps !! i))) | i <- [0..n-1] ]

          allF      = tensionForces ++ repulsions ++ gravs
          forceAt i = foldl' (+^) (0,0) [f | (j,f) <- allF, j==i]
          ps'       = [ (ps !! i) +^ clamp (forceAt i) | i <- [0..n-1] ]
      in go (k-1) ps'

runLayout :: ForceParams -> [(Int,Int)] -> [V2] -> [V2]
runLayout p es p0 = go (steps p) (temp0 p) p0
  where
    go 0 _   ps = ps
    go k tmp ps = let ps' = step (p { temp0 = tmp }) es ps
                  in go (k-1) (tmp * cool p) ps'

--------------------------------------------------------------------------------
-- 六角形（pointy-top）とドア＝辺中点
--------------------------------------------------------------------------------

angles :: [Double]
angles = [pi/2 - fromIntegral k * pi/3 | k <- [0..5]]

hexVertices :: Double -> V2 -> [V2]
hexVertices r (cx,cy) =
  [ (cx + r * cos a, cy + r * sin a) | a <- angles ]

edgeMids :: [V2] -> [V2]
edgeMids vs =
  [ let (x1,y1) = vs !! i; (x2,y2) = vs !! ((i+1) `mod` 6)
    in ((x1+x2)/2, (y1+y2)/2) | i <- [0..5] ]

doorPoint :: Double -> V2 -> Door -> V2
doorPoint r c d = edgeMids (hexVertices r c) !! (d `mod` 6)

edgeTangent :: Double -> V2 -> Door -> V2
edgeTangent r c d =
  let vs = hexVertices r c; a = vs !! (d `mod` 6); b = vs !! ((d+1) `mod` 6)
  in unit (b -^ a)

--------------------------------------------------------------------------------
-- 幾何：ベジエのサンプリングで安全確認
--------------------------------------------------------------------------------

distPoint :: V2 -> V2 -> Double
distPoint (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

cubicAt :: V2 -> V2 -> V2 -> V2 -> Double -> V2
cubicAt p1 c1 c2 p2 t =
  let u  = 1 - t
      uu = u*u; tt = t*t
      a = (uu*u) *^ p1
      b = (3*uu*t) *^ c1
      c = (3*u*tt) *^ c2
      d = (tt*t)   *^ p2
  in a +^ b +^ c +^ d

safeCubic :: [(Int,V2)] -> Int -> Int -> Double -> Double
          -> V2 -> V2 -> V2 -> V2 -> Bool
safeCubic centers i j rad safety p1 c1 c2 p2 =
  let r = rad + safety
      ts = [0.10,0.20..0.90]   -- 端点は除外
      okFor (k,c) =
        if k == i || k == j then True
        else all (\t -> distPoint (cubicAt p1 c1 c2 p2 t) c >= r) ts
  in all okFor centers

findOffsetCubic
  :: [(Int,V2)] -> Int -> Int -> Double -> Double
  -> V2 -> V2 -> V2 -> V2 -> V2 -> Double -> Double
findOffsetCubic centers i j rad safety p1 c1b c2b p2 nperp side =
  let base   = 0.6 * rad
      grow a = a * 1.25
      maxTry = 18 :: Int
      try a k
        | k >= maxTry = a
        | otherwise =
            let c1 = c1b +^ ((a*side) *^ nperp)
                c2 = c2b +^ ((a*side) *^ nperp)
            in if safeCubic centers i j rad safety p1 c1 c2 p2
               then a else try (grow a) (k+1)
  in try base 0

--------------------------------------------------------------------------------
-- SVG 生成（Edgeのみ。自己ループは“葉”）
--------------------------------------------------------------------------------

layoutToSVG
  :: Double -> Double -> Double -> Int -> Layout -> String
layoutToSVG initRadius rad margin iters (labels, _start, edges) =
  let n = length labels
      idealLen  = 3.2 * rad
      p = ForceParams { steps=iters, temp0=idealLen*0.45, cool=0.96
                      , kRep=(idealLen*idealLen)*0.08, kSpring=0.25
                      , gravity=0.000, minSep=2*rad+0.4*rad }
      p0      = initialOnCircle n initRadius
      ers     = [ (ri,rj) | ((ri,_),(rj,_)) <- edges, ri /= rj ]
      placed0 = runLayout p ers p0
      placed1 = relaxByEdges rad 60 0.12 edges placed0

      scaleTarget = idealLen * 7.0 -- 3.6
      (centers, w0, h0) = scaleAndPad margin scaleTarget placed1
      w = w0 + margin; h = h0 + margin
      centersI = zip [0..] centers

      -- そのドアが使われているか（自己ループ含む）
      usedDoor :: Int -> Int -> Bool
      usedDoor ri di =
        any (\((a,da),(b,db)) -> (a==ri && da==di) || (b==ri && db==di)) edges

      -- ドア番号の描画：線色とは独立してグレー系に
      doorLabelText :: Int -> V2 -> V2 -> V2 -> Int -> String
      doorLabelText ri p nVec tVec d =
        -- 位置は辺のすぐ外側＋接線方向に少し逃がす
        let pos = p +^ ((0.42 * rad) *^ nVec) +^ ((0.12 * rad) *^ tVec)
            (x,y) = pos
            colUsed   = "#666"
            colUnused = "#e22"
            col = if usedDoor ri d then colUsed else colUnused
        in "<text x=\"" ++ fmt2 x ++ "\" y=\"" ++ fmt2 y
           ++ "\" font-size=\"11\" font-weight=\"600\" text-anchor=\"middle\" "
           ++ "paint-order=\"stroke\" stroke=\"#fff\" stroke-width=\"1.2\" "
           ++ "fill=\"" ++ col ++ "\">"
           ++ show d ++ "</text>"

      roomSvg i c =
        let vs  = hexVertices rad c
            pts = intercalate " " [ fmt2 x ++ "," ++ fmt2 y | (x,y) <- vs ]
            rl  = labels !! i
            (cx,cy) = c; tx = cx; ty = cy + 3.5
            doorTexts =
              let one d = let p  = doorPoint rad c d
                              nV = unit (p -^ c)
                              tV = edgeTangent rad c d
                          in doorLabelText i p nV tV d
              in concatMap (\d -> [one d]) [0..5]
        in [ "<polygon points=\"" ++ pts ++ "\" fill=\"white\" stroke=\"#333\" stroke-width=\"1.1\"/>"
           , "<text x=\"" ++ fmt2 tx ++ "\" y=\"" ++ fmt2 ty
             ++ "\" font-size=\"10\" text-anchor=\"middle\" fill=\"#111\">"
             ++ ("R" ++ show i ++ " / L" ++ show rl) ++ "</text>"
           ] ++ doorTexts

      roomsSvg = concat [ roomSvg i c | (i,c) <- zip [0..] centers ]

      -- 自己ループ（同一ドア）は“葉”1本ベジエ
      selfLoopLeaf :: Int -> Door -> V2 -> String
      selfLoopLeaf ri di p =
        let ci = centers !! ri; t = edgeTangent rad ci di; nO = unit (p -^ ci)
            a = 0.65*rad; b = 0.50*rad
            c1 = p +^ ((a *^ t)) +^ ((b *^ nO))
            c2 = p -^ ((a *^ t)) +^ ((b *^ nO))
        in "M " ++ pt p ++ " C " ++ pt c1 ++ " " ++ pt c2 ++ " " ++ pt p

      -- 1 本の cubic Bézier：左右案を作り、上空侵入ペナルティが小さい方を採用
      singleCubicPath :: Int -> Int -> V2 -> V2 -> (V2,V2) -> (V2,V2) -> String
      singleCubicPath i j p1 p2 (n1,_t1) (n2,_t2) =
        let out1 = 0.95 * rad
            out2 = 0.95 * rad
            h1b  = p1 +^ (out1 *^ n1)
            h2b  = p2 +^ (out2 *^ n2)
            dir  = unit (p2 -^ p1)
            nperp= perp dir
            safety = 0.40 * rad
            (ii,jj) = if i == j then (-1,-2) else (i,j)

            build side =
              let off   = findOffsetCubic centersI ii jj rad safety p1 h1b h2b p2 nperp side
                  off1  = 0.35 * off
                  c1b   = h1b +^ ((off1*side) *^ nperp)
                  c2b   = h2b +^ ((off*side)  *^ nperp)
                  -- 入口/出口の室内侵入を見たら外向き成分だけ増やす
                  ci = centers !! i; cj = centers !! j
                  tsS = [0.04,0.08,0.12]; tsE = [0.88,0.92,0.96]
                  grows k = k*1.22; maxTry=12::Int
                  okS len = all (\t -> distPoint (cubicAt p1 (p1 +^ (len *^ n1) +^ ((off1*side) *^ nperp)) c2b p2 t) ci >= rad + 0.18*rad) tsS
                  okE len = all (\t -> distPoint (cubicAt p1 c1adj (p2 +^ (len *^ n2) +^ ((off *side) *^ nperp)) p2 t)  cj >= rad + 0.18*rad) tsE
                  adjS len k
                    | k>=maxTry = p1 +^ (len *^ n1) +^ ((off1*side) *^ nperp)
                    | okS len   = p1 +^ (len *^ n1) +^ ((off1*side) *^ nperp)
                    | otherwise = adjS (grows len) (k+1)
                  c1adj = adjS out1 0
                  adjE len k
                    | k>=maxTry = p2 +^ (len *^ n2) +^ ((off *side) *^ nperp)
                    | okE len   = p2 +^ (len *^ n2) +^ ((off *side) *^ nperp)
                    | otherwise = adjE (grows len) (k+1)
                  c2adj = adjE out2 0
              in (c1adj,c2adj)

            (c1p,c2p) = build   1.0
            (c1m,c2m) = build (-1.0)

            -- 上空侵入ペナルティ（部屋中心より上側かつ半径2.2*rad以内）
            airPenalty (c1,c2) =
              let ts = [0.05,0.10..0.95]
                  pen center =
                    let (_,cy) = center
                    in sum [ if py < cy && distPoint p center < 2.2*rad then (1::Int) else 0
                           | t <- ts, let p@(_,py) = cubicAt p1 c1 c2 p2 t ]
              in fromIntegral (pen (centers !! i) + pen (centers !! j)) :: Double

            -- 予備：部屋からの最近距離（大きい方を好む）
            clearance (c1,c2) =
              let ts = [0.05,0.10..0.95]
                  ds = [ min (distPoint (cubicAt p1 c1 c2 p2 t) (centers !! i))
                               (distPoint (cubicAt p1 c1 c2 p2 t) (centers !! j))
                        | t <- ts ]
              in minimum ds

            pPen = airPenalty (c1p,c2p)
            mPen = airPenalty (c1m,c2m)
            (c1,c2) =
              if pPen < mPen then (c1p,c2p)
              else if mPen < pPen then (c1m,c2m)
              else if clearance (c1p,c2p) >= clearance (c1m,c2m)
                   then (c1p,c2p) else (c1m,c2m)
        in "M " ++ pt p1 ++ " C " ++ pt c1 ++ " " ++ pt c2 ++ " " ++ pt p2

      edgePath ((ri,di),(rj,dj))
        | ri == rj && di == dj =
            let ci = centers !! ri; p  = doorPoint rad ci di
            in "<path d=\"" ++ selfLoopLeaf ri di p
               ++ "\" fill=\"none\" stroke=\"#06c\" stroke-width=\"1.6\" stroke-opacity=\"0.9\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>"
        | otherwise =
            let ci = centers !! ri; cj = centers !! rj
                p1 = doorPoint rad ci di; p2 = doorPoint rad cj dj
                n1 = unit (p1 -^ ci);    n2 = unit (p2 -^ cj)
                t1 = edgeTangent rad ci di; t2 = edgeTangent rad cj dj
                d  = singleCubicPath ri rj p1 p2 (n1,t1) (n2,t2)
            in "<path d=\"" ++ d
               ++ "\" fill=\"none\" stroke=\"#06c\" stroke-width=\"1.6\" stroke-opacity=\"0.9\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>"

      edgesSvg = map edgePath edges
      header = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ++ fmt2 w
            ++ "\" height=\"" ++ fmt2 h
            ++ "\" viewBox=\"0 0 " ++ fmt2 w ++ " " ++ fmt2 h ++ "\">"
      footer = "</svg>"
  in unlines $ [header, "<rect x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" fill=\"white\"/>"]
            ++ roomsSvg ++ edgesSvg ++ [footer]

--------------------------------------------------------------------------------
-- スケール & パディング
--------------------------------------------------------------------------------

scaleAndPad :: Double -> Double -> [V2] -> ([V2], Double, Double)
scaleAndPad pad scaleTarget ps =
  let xs = map fst ps; ys = map snd ps
      minX = minimum xs; maxX = maximum xs
      minY = minimum ys; maxY = maximum ys
      w = max 1e-9 (maxX-minX); h = max 1e-9 (maxY-minY)
      s = scaleTarget / max w h
      shifted = [ ((x - minX)*s + pad, (y - minY)*s + pad) | (x,y) <- ps ]
  in (shifted, w*s + pad*2, h*s + pad*2)



--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

layoutToSVGFile :: FilePath -> Layout -> IO ()
layoutToSVGFile path layout = do
  let rad = 26
  let svg = layoutToSVG (6*rad) rad 140 420 layout
  writeFile path svg


guessRequestMapToLayout :: GuessRequestMap -> Layout
guessRequestMapToLayout (GuessRequestMap roomLabels startingRooms connects)
  = (roomLabels, startingRooms, [ ((a,b),(c,d)) | (Connection (RoomDoor a b) (RoomDoor c d)) <- connects ])


guessToSVGFile :: FilePath -> IO ()
guessToSVGFile fp = do
  bs <- BL.readFile fp -- "solutions/secundus/07-2041.guess"
  case eitherDecode bs of
    Left err -> die $ "JSON decode error: " ++ err
    Right guessRequestMap -> do
      let layout = guessRequestMapToLayout guessRequestMap
      layoutToSVGFile "guess_layout.svg" layout

--------------------------------------------------------------------------------
-- 例 & 実行
--------------------------------------------------------------------------------

example :: Layout
example =
  ( [0,1,2,0]
  , 4
  , [ ((0,0), (1,5))
    , ((0,1), (0,1))
    , ((0,2), (3,3))
    , ((0,3), (2,5))
    , ((0,4), (0,5))
    , ((1,0), (1,0))
    , ((1,1), (3,0))
    , ((1,2), (1,2))
    , ((1,3), (2,0))
    , ((1,4), (3,5))
    , ((2,1), (2,3))
    , ((2,2), (2,2))
    , ((2,4), (2,4))
    , ((3,1), (3,1))
    , ((3,2), (3,2))
    ]
  )

main :: IO ()
main = do
  let rad = 36
  let svg = layoutToSVG (9*rad) rad 140 420 example
  writeFile "layout.svg" svg
