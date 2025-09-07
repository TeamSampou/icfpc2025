{-# LANGUAGE ScopedTypeVariables #-}
module LayoutHexSVG where

import Data.List (intercalate, foldl')
import Numeric   (showFFloat)

-- 型
type RoomLabel = Int
type RoomIndex = Int
type Door      = Int
type Layout = ([RoomLabel], RoomIndex, [((RoomIndex, Door), (RoomIndex, Door))])

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

--------------------------------------------------------------------------------
-- 力学レイアウト（コンパクト寄り）
--------------------------------------------------------------------------------

data ForceParams = ForceParams
  { steps   :: Int
  , temp0   :: Double
  , cool    :: Double
  , kRep    :: Double
  , kSpring :: Double
  , gravity :: Double
  , minSep  :: Double
  }

initialOnCircle :: Int -> Double -> [V2]
initialOnCircle n radius =
  [ (radius * cos t, radius * sin t)
  | i <- [0..n-1]
  , let t = 2*pi*fromIntegral i / fromIntegral (max 1 n)
  ]

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
                         then ((minSep p - dist) * 2.0) *^ unit d
                         else (0,0)
            in rep +^ sep
          | j <- [0..n-1], j /= i
          , let pi = ps !! i
          , let pj = ps !! j
          ]

      fSpring i =
        foldl' (+^) (0,0)
          [ let d    = pj -^ pi
                dist = norm d
                rest = 0.6 * minSep p
                pull = (kSpring p * (dist - rest)) *^ unit d
            in pull
          | (u,v) <- es
          , i == u || i == v
          , let pi = ps !! i
          , let pj = ps !! (if i == u then v else u)
          ]

      fGrav i =
        let pi  = ps !! i
            dir = ctr -^ pi
        in gravity p *^ dir

      t     = temp0 p
      force = [ fRep i +^ fSpring i +^ fGrav i | i <- [0..n-1] ]
      move  = [ let m = norm f in if m > t then (t/m) *^ f else f | f <- force ]
  in zipWith (+^) ps move

runLayout :: ForceParams -> [(Int,Int)] -> [V2] -> [V2]
runLayout p es p0 = go (steps p) (temp0 p) p0
  where
    go 0 _   ps = ps
    go k tmp ps =
      let ps' = step (p { temp0 = tmp }) es ps
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
  [ let (x1,y1) = vs !! i
        (x2,y2) = vs !! ((i+1) `mod` 6)
    in ((x1+x2)/2, (y1+y2)/2)
  | i <- [0..5]
  ]

doorPoint :: Double -> V2 -> Door -> V2
doorPoint r c d = edgeMids (hexVertices r c) !! (d `mod` 6)

-- 辺 d の接線方向（v_d→v_{d+1}）
edgeTangent :: Double -> V2 -> Door -> V2
edgeTangent r c d =
  let vs = hexVertices r c
      a  = vs !! (d `mod` 6)
      b  = vs !! ((d+1) `mod` 6)
  in unit (b -^ a)

--------------------------------------------------------------------------------
-- 衝突判定 & ベジエ制御点
--------------------------------------------------------------------------------

segIntersects :: V2 -> V2 -> V2 -> V2 -> Bool
segIntersects (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
  let d = (x2-x1)*(y4-y3) - (y2-y1)*(x4-x3)
  in if d == 0 then False
     else
       let s = ((x3-x1)*(y4-y3) - (y3-y1)*(x4-x3)) / d
           t = ((x3-x1)*(y2-y1) - (y3-y1)*(x2-x1)) / d
       in s>=0 && s<=1 && t>=0 && t<=1

segmentHitsHex :: Double -> V2 -> V2 -> V2 -> Bool
segmentHitsHex r c p1 p2 =
  let vs = hexVertices r c
      es = [ (vs!!i, vs!!((i+1) `mod` 6)) | i <- [0..5] ]
  in any (\(a,b)-> segIntersects p1 p2 a b) es

needsDetour :: Double -> [V2] -> V2 -> V2 -> Bool
needsDetour r centers p1 p2 =
  any (\c -> segmentHitsHex r c p1 p2) centers

-- ベジエ：端点外向き + ベース曲げ + 必要に応じて増量
bezierControls
  :: Double -> Double -> Double -> [V2]
  -> V2 -> V2 -> V2 -> V2 -> (V2,V2)
bezierControls r outBias bend centers ci cj p1 p2 =
  let v1 = unit (p1 -^ ci)
      v2 = unit (p2 -^ cj)
      baseC1 = p1 +^ (outBias *^ v1)
      baseC2 = p2 +^ (outBias *^ v2)
      seg    = p2 -^ p1
      nPerp  = unit (perp seg)
      det    = needsDetour r centers p1 p2
      base   = 0.25 * r                                 -- 常に少し曲げる
      detAmt = base + (if det then 1.1 else 0.6) * bend
      -- 左右ばらし
      h    = round (abs (fst p1 + snd p1 + fst p2 + snd p2) * 31) :: Int
      side = if even h then 1.0 else -1.0
      off  = (detAmt * side) *^ nPerp
  in (baseC1 +^ off, baseC2 +^ off)

--------------------------------------------------------------------------------
-- ユーティリティ
--------------------------------------------------------------------------------

edgeRoomPairs :: [((RoomIndex,Door),(RoomIndex,Door))] -> [(Int,Int)]
edgeRoomPairs es = [ (ri,rj) | ((ri,_),(rj,_)) <- es, ri /= rj ]

scaleAndPad :: Double -> Double -> [V2] -> ([V2], Double, Double)
scaleAndPad pad scaleTarget ps =
  let xs = map fst ps; ys = map snd ps
      minX = minimum xs; maxX = maximum xs
      minY = minimum ys; maxY = maximum ys
      w = max 1e-9 (maxX-minX)
      h = max 1e-9 (maxY-minY)
      s = scaleTarget / max w h
      shifted = [ ((x - minX)*s + pad, (y - minY)*s + pad) | (x,y) <- ps ]
  in (shifted, w*s + pad*2, h*s + pad*2)

--------------------------------------------------------------------------------
-- SVG 生成（Edge のみ・自己ループは“葉”）
--------------------------------------------------------------------------------

layoutToSVG
  :: Double   -- 初期円半径（例: 6*rad）
  -> Double   -- 六角半径 rad
  -> Double   -- 余白 margin
  -> Int      -- レイアウト反復 steps
  -> Layout
  -> String
layoutToSVG initRadius rad margin iters (labels, _start, edges) =
  let n = length labels

      idealLen  = 3.2 * rad
      p = ForceParams
            { steps   = iters
            , temp0   = idealLen * 0.45
            , cool    = 0.96
            , kRep    = (idealLen*idealLen) * 0.08
            , kSpring = 0.25
            , gravity = 0.015
            , minSep  = 2*rad + 0.4*rad
            }

      p0      = initialOnCircle n initRadius
      ers     = edgeRoomPairs edges
      placed0 = runLayout p ers p0
      (centers, w, h) = scaleAndPad margin (idealLen * 3.8) placed0

      -- 六角 + ラベル
      roomSvg i c =
        let vs  = hexVertices rad c
            pts = intercalate " " [ fmt2 x ++ "," ++ fmt2 y | (x,y) <- vs ]
            rl  = labels !! i
            (cx,cy) = c
            tx = cx; ty = cy + 3.5
        in [ "<polygon points=\"" ++ pts ++ "\" fill=\"none\" stroke=\"#333\" stroke-width=\"1\"/>"
           , "<text x=\"" ++ fmt2 tx ++ "\" y=\"" ++ fmt2 ty
             ++ "\" font-size=\"10\" text-anchor=\"middle\" fill=\"#111\">"
             ++ ("R" ++ show i ++ " / L" ++ show rl) ++ "</text>"
           ]

      roomsSvg = concat [ roomSvg i c | (i,c) <- zip [0..] centers ]

      -- 自己ループ：ドア p を始点・終点にして、接線 ±a と外向き +b に制御点
      selfLoopPath :: Door -> V2 -> V2 -> String
      selfLoopPath di ci p =
        let t  = edgeTangent rad ci di         -- 辺の接線
            nO = unit (p -^ ci)                -- 外向き法線（中心→ドア）
            a  = 0.65 * rad                    -- 接線方向の伸び
            b  = 0.48 * rad                    -- 外向きへのふくらみ
            c1 = p +^ (a *^ t) +^ (b *^ nO)
            c2 = p -^ (a *^ t) +^ (b *^ nO)
        in  "M " ++ fmt2 (fst p) ++ " " ++ fmt2 (snd p)
         ++ " C " ++ fmt2 (fst c1) ++ " " ++ fmt2 (snd c1)
         ++ ", "  ++ fmt2 (fst c2) ++ " " ++ fmt2 (snd c2)
         ++ ", "  ++ fmt2 (fst p)  ++ " " ++ fmt2 (snd p)

      -- 通常エッジ：端点＝辺中点、Cubic Bezier（曲げは常に少し＋必要なら増量）
      edgePath ((ri,di),(rj,dj))
        | ri == rj =
            let ci = centers !! ri
                p  = doorPoint rad ci di
            in "<path d=\"" ++ selfLoopPath di ci p
               ++ "\" fill=\"none\" stroke=\"#06c\" stroke-width=\"1.6\" stroke-opacity=\"0.9\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>"
        | otherwise =
            let ci = centers !! ri
                cj = centers !! rj
                p1 = doorPoint rad ci di
                p2 = doorPoint rad cj dj
                (c1,c2) = bezierControls rad (0.85*rad) (0.55*rad) centers ci cj p1 p2
            in "<path d=\"M " ++ fmt2 (fst p1) ++ " " ++ fmt2 (snd p1)
               ++ " C "  ++ fmt2 (fst c1) ++ " " ++ fmt2 (snd c1)
               ++ ", "    ++ fmt2 (fst c2) ++ " " ++ fmt2 (snd c2)
               ++ ", "    ++ fmt2 (fst p2) ++ " " ++ fmt2 (snd p2)
               ++ "\" fill=\"none\" stroke=\"#06c\" stroke-width=\"1.6\" stroke-opacity=\"0.9\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>"

      edgesSvg = map edgePath edges

      header = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ++ fmt2 w
            ++ "\" height=\"" ++ fmt2 h
            ++ "\" viewBox=\"0 0 " ++ fmt2 w ++ " " ++ fmt2 h ++ "\">"
      footer = "</svg>"
  in unlines $ [header, "<rect x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" fill=\"white\"/>"]
            ++ roomsSvg
            ++ edgesSvg
            ++ [footer]

--------------------------------------------------------------------------------
-- 例 & 実行
--------------------------------------------------------------------------------

example :: Layout
example =
  ( [0,1,2,1]
  , 0
  , [ ((0,3),(1,2))
    , ((1,4),(3,1))
    , ((3,5),(2,0))
    , ((2,2),(0,1))   -- クロス
    , ((0,0),(0,5))   -- 自己ループ（0）
    , ((1,0),(1,0))   -- 自己ループ（1）
    , ((2,3),(2,4))   -- 自己ループ（2）
    , ((3,2),(3,2))   -- 自己ループ（3）
    ]
  )

main :: IO ()
main = do
  let rad = 26
  let svg = layoutToSVG (6*rad) rad 28 380 example
  writeFile "layout.svg" svg
