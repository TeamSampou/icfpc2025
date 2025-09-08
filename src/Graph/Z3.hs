module Graph.Z3
  ( findGraph
  ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IORef
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Z3.Monad as Z3

import Base
import Graph (DiGraph)
import ObservationSummary (ObservationSummary, Trie (..))
import qualified ObservationSummary as Trie


findGraph :: Int -> ObservationSummary -> IO (Maybe (DiGraph, RoomIndex))
findGraph numRooms t = Z3.evalZ3 $ findGraph' numRooms t

findGraph' :: forall z3. Z3.MonadZ3 z3 => Int -> ObservationSummary -> z3 (Maybe (DiGraph, RoomIndex))
findGraph' numRooms t@(Node startingRoomLabel _ _) = do
  -- Finite-domain sort は振る舞いが怪しいので代わりにIntを用いる
  let useIntSort = True
  -- 各ラベルについて floor (numRooms / 4) 個の部屋はあると仮定
  let assumeBalancedLabelDistribution = False

  sBool <- Z3.mkBoolSort

  sRoom <-
    if useIntSort then do
      Z3.mkIntSort
    else do
      symRoom <- Z3.mkStringSymbol "Room"
      Z3.mkFiniteDomainSort symRoom (fromIntegral numRooms)
  rooms <- forM [0..numRooms-1] $ \i -> Z3.mkInt i sRoom
  let startingRoom = rooms !! 0

  doorFuncs <- forM [(0::Int)..5] $ \d -> do
    sym <- Z3.mkStringSymbol ("d" ++ show d)
    func <- Z3.mkFuncDecl sym [sRoom] sRoom
    when useIntSort $ do
      forM_ rooms $ \r -> do
        r2 <- Z3.mkApp func [r]
        lb <- Z3.mkInt 0 sRoom
        ub <- Z3.mkInt (numRooms - 1) sRoom
        Z3.solverAssertCnstr =<< Z3.mkLe lb r2
        Z3.solverAssertCnstr =<< Z3.mkLe r2 ub
    return func

  if True then do
    -- 対称性を厳密に保証する

    -- e[i,j](r) は部屋 r からドア i を通った際の出口側のドアは j であるということを表す論理変数
    -- すなわち i で行ったら j で返ってこれる。
    e <- fmap (Map.fromList . concat) $ forM [0..5] $ \i -> do
      forM [0..5] $ \j -> do
        sym <- Z3.mkStringSymbol ("e" ++ show i ++ show j)
        func <- Z3.mkFuncDecl sym [sRoom] sBool
        pure ((i,j), func)

    -- 各r,iについて e[i,j] を満たす j はただ一つ
    forM_ rooms $ \r -> do
      forM_ [0..5] $ \i -> do
        cs <- forM [0..5] $ \j -> Z3.mkApp (e Map.! (i,j)) [r]
        Z3.solverAssertCnstr =<< Z3.mkOr cs
        forM_ (pairs cs) $ \(c1,c2) -> do
          tmp <- Z3.mkAnd [c1,c2]
          Z3.solverAssertCnstr =<< Z3.mkNot tmp

    -- e[i,j](r) → d[j](d[i](r))=r ∧ e[j,i](d[i](r))
    -- ちゃんと戻ってこれて、ドア同士が対応している
    forM_ rooms $ \r -> do
      forM_ [0..5] $ \i -> do
        forM_ [0..5] $ \j -> do
          premise <- Z3.mkApp (e Map.! (i,j)) [r]
          -- i で行った先の部屋
          r2 <- Z3.mkApp (doorFuncs !! i) [r]
          -- i で行ったら j で戻ってこれる。すなわち d[j](r2)=r
          conclusion1 <- do
            r3 <- Z3.mkApp (doorFuncs !! j) [r2]
            Z3.mkEq r r3
          -- 戻ってくる際のドアの対応はj,i。すなわち e[j,i](r2)
          conclusion2 <- Z3.mkApp (e Map.! (j,i)) [r2]
          -- 最終的な条件
          conclusion <- Z3.mkAnd [conclusion1, conclusion2]
          Z3.solverAssertCnstr =<< Z3.mkImplies premise conclusion

  else do
    -- 部屋の間のエッジは逆向きのエッジが存在しないといけない。
    -- 本当は本数まであっていないといけないが、ここでは存在だけを制約にする。
    forM_ rooms $ \room -> do
      forM_ doorFuncs $ \df -> do
         -- d_1(d_i(room))=room ∨ … ∨ d_n(d_i(room))=room
         room2 <- Z3.mkApp df [room]
         cs <- forM doorFuncs $ \df2 -> do
           room3 <- Z3.mkApp df2 [room2]
           Z3.mkEq room room3
         Z3.solverAssertCnstr =<< Z3.mkOr cs

  sLabel <- Z3.mkBvSort 2

  let fixedLabels
        | assumeBalancedLabelDistribution =
            let m = numRooms `div` 4
             in startingRoomLabel : concat [replicate (if label == startingRoomLabel then m - 1 else m) label | label <- [0..3]]
        | otherwise =
            startingRoomLabel : IntSet.toList (IntSet.delete startingRoomLabel (Trie.collectUnmodifiedLabels t))
  startingLabels <- forM [0..numRooms-1] $ \i -> do
    if i < length fixedLabels then
      Z3.mkInt (fixedLabels !! i) sLabel
    else do
      sym <- Z3.mkStringSymbol $ "starting_label_" ++ show i
      Z3.mkVar sym sLabel
  -- symmetry breaking
  forM_ [length fixedLabels .. numRooms - 2] $ \i -> do
    Z3.solverAssertCnstr =<< Z3.mkBvule (startingLabels !! i) (startingLabels !! (i+1))

  -- Trieをトラバースして、各ラベルについて最初に見つかった頂点は、ラベルを固定した部屋だとしても一般性を失わない
  do seenLabelsRef <- liftIO $ newIORef (IntSet.singleton startingRoomLabel)
     let preprocess :: Z3.AST -> ObservationSummary -> z3 ()
         preprocess currentRoom (Node label childrenD _childrenL) = do
           seenLabels <- liftIO $ readIORef seenLabelsRef
           unless (label `IntSet.member` seenLabels) $ do
             case elemIndex label fixedLabels of
               Nothing -> pure ()
               Just i -> do
                 iExpr <- Z3.mkInt i sRoom
                 Z3.solverAssertCnstr =<< Z3.mkEq currentRoom iExpr
             liftIO $ writeIORef seenLabelsRef (IntSet.insert label seenLabels)
           forM_ (IntMap.toList childrenD) $ \(d, ch) -> do
             newRoom <- Z3.mkApp (doorFuncs !! d) [currentRoom]
             preprocess newRoom ch
     preprocess startingRoom t

  -- Finite-domain sort は Array の index に使うとうまく機能しないので、代わりに場合分けで書く
  let select :: [Z3.AST] -> Z3.AST -> z3 Z3.AST
      select xs key = do
        foldM (\expr i -> do
                  iExpr <- Z3.mkInt i sRoom
                  cond <- Z3.mkEq key iExpr
                  Z3.mkIte cond (xs !! i) expr)
              (xs !! (numRooms - 1))
              [numRooms-2, numRooms-3 .. 0]

      store :: [Z3.AST] -> Z3.AST -> Z3.AST -> z3 [Z3.AST]
      store xs key val = do
        forM (zip [0..] xs) $ \(i,x) -> do
          iExpr <- Z3.mkInt i sRoom
          cond <- Z3.mkEq key iExpr
          Z3.mkIte cond val x

  let f :: Seq Action -> Z3.AST -> [Z3.AST] -> ObservationSummary -> z3 ()
      f hist currentRoom currentLabels (Node label childrenD childrenL) = do
        currentLabelVar <- select currentLabels currentRoom
        currentLabelVal <- Z3.mkInt label sLabel
        Z3.solverAssertCnstr =<< Z3.mkEq currentLabelVar currentLabelVal
        -- tmp <- Z3.astToString =<< Z3.mkEq currentLabelVar currentLabelVal
        -- liftIO $ putStrLn $ "assert: " ++ tmp

        forM_ (IntMap.toList childrenD) $ \(d, ch) -> do
          newRoom <- Z3.mkApp (doorFuncs !! d) [currentRoom]
          f (hist Seq.|> PassDoor d) newRoom currentLabels ch

        forM_ (IntMap.toList childrenL) $ \(l, ch) -> do
          newLabelVal <- Z3.mkInt l sLabel
          newLabels <- store currentLabels currentRoom newLabelVal
          f (hist Seq.|> AlterLabel l) currentRoom newLabels ch

  f Seq.empty startingRoom startingLabels t

  forM_ (Trie.deriveNontrivialDisequalities t) $ \(p1, p2) -> do
    let g :: Z3.AST -> Char -> z3 Z3.AST
        g e c = do
          let d = read [c]
          Z3.mkApp (doorFuncs !! d) [e]
    e1 <- foldM g startingRoom p1
    e2 <- foldM g startingRoom p2
    Z3.solverAssertCnstr =<< Z3.mkNot =<< Z3.mkEq e1 e2

  ret <- Z3.solverCheck

  if ret /= Z3.Sat then do
    pure Nothing
  else do
    m <- Z3.solverGetModel
    -- str <- Z3.modelToString m
    -- liftIO $ putStrLn str

    g <- fmap V.fromList $ forM [0..numRooms-1] $ \r -> do
      let room = rooms !! r

      label <- fmap (fromIntegral . fromJust) $ Z3.evalInt m (startingLabels !! r)

      outEdges <- fmap IntMap.fromList $ forM (zip [0..] doorFuncs) $ \(door, df) -> do
        destExpr <- Z3.mkApp df [room]
        dest <- fmap (fromIntegral . fromJust) $ Z3.evalInt m destExpr
        pure (door, dest)

      pure (label, outEdges)

    return (Just (g, 0))


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs


-- test case for "probatio"
_test1 = findGraph 3 t
  where
    t = Trie.fromObservations $ zip plans results
    plans = ["000","123","213","333"] ++ ["4","5","02","03","04","05","31","32","34","35"]
    results = [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]] ++ [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]

-- test case for "primus"
_test2 = findGraph 6 t
  where
    t = Trie.fromObservation plan result
    plan = "021320403505044123550520034431312210134541025332505010033554343013423011254052531011533004340304253205132534"
    result = [0,0,3,0,0,3,3,3,3,2,3,3,1,2,3,3,0,3,2,3,1,2,3,2,0,0,0,2,3,2,0,0,0,0,0,0,0,0,0,2,3,3,0,1,1,1,0,0,3,1,2,3,3,0,1,1,1,0,0,0,2,1,1,1,1,1,0,2,1,1,1,1,2,1,1,1,2,3,2,3,2,0,1,2,0,0,0,0,1,1,3,2,3,3,2,0,2,1,1,0,3,3,1,1,0,3,1,1,1]

-- test case for "secundus"
_test3 = findGraph 12 t
  where
    t = Trie.fromObservations $ zip plans results
    plans =
      [ "205453044150242021134201232313140114532311403002015055315120434051505411210234055145550135005250420542531311152251205410534135504020204421105423154114445040134224053345420250520251533405521522155000534233441515505504"
      , "435131021401243542552323244440021455152551451033444503251113531545214043545145041444541553454340254001543412412352133131000251212054004522351501422005301411013322014531512252541532523315144543305302004142024312553022"
      , "014103511215141312305323252342322235214531434042525532110523445032451304013520554242433313050232041103421223212411115241323323432034111153443051150454203404154552033513314320100420523050434415242135525503142022053521"
      ]
    results =
      [ [0,1,0,2,0,1,3,3,1,1,2,0,1,0,3,1,3,1,1,0,0,0,0,1,2,1,3,3,2,3,3,1,3,2,3,2,0,1,3,1,3,1,1,1,1,3,2,3,1,3,1,0,2,0,2,3,3,2,3,1,3,2,3,2,1,0,1,0,2,0,0,0,1,0,1,1,1,3,2,3,1,0,1,2,0,1,0,2,2,2,0,2,1,0,1,0,2,2,1,1,0,0,0,2,3,3,2,3,2,2,0,1,1,0,1,0,2,0,2,2,3,0,3,1,0,0,1,0,2,2,3,1,3,1,3,2,0,0,0,1,1,0,0,0,0,0,1,3,2,2,0,3,2,0,1,2,1,2,2,2,1,0,3,3,0,3,3,2,0,2,1,1,1,0,2,0,1,0,1,0,0,1,3,0,3,3,0,1,1,1,0,0,1,0,2,0,1,1,0,2,3,2,3,2,3,2,2,2,0,1,0,1,0,2,0,1,2]
      , [0,2,2,0,0,2,2,3,1,1,1,1,0,1,1,3,2,0,0,1,0,0,0,0,0,0,2,0,3,2,3,3,1,1,1,0,2,3,0,0,2,0,1,1,0,1,0,0,3,1,1,2,0,2,2,3,2,3,2,2,2,0,3,2,0,2,0,1,0,2,3,1,3,0,0,1,0,2,0,1,2,2,2,0,3,1,3,2,0,2,3,2,0,0,3,1,0,1,0,0,2,1,2,0,2,2,2,2,1,2,2,3,2,0,1,0,0,3,2,2,2,1,1,0,1,0,0,0,0,0,2,0,0,2,1,2,0,0,1,3,0,0,1,3,1,3,1,3,3,2,0,0,2,2,2,2,2,3,2,2,2,3,3,1,2,0,1,3,1,0,1,0,0,1,1,0,3,1,0,2,1,0,1,3,3,1,0,1,2,2,0,2,2,2,1,0,0,2,3,1,1,1,2,0,0,2,3,2,2,2,3,2,0,0,2,3,3]
      , [0,1,2,0,1,1,3,0,0,1,0,1,0,1,1,2,2,2,3,2,3,1,3,1,3,1,0,0,3,1,1,3,1,3,1,3,1,3,1,3,0,3,2,0,2,0,2,0,0,1,1,0,1,3,1,1,0,1,0,1,3,1,1,0,1,3,3,2,0,0,2,3,1,0,1,3,0,0,1,0,2,0,0,0,0,2,2,2,2,2,2,1,0,1,0,2,1,1,1,2,2,3,3,2,3,3,3,3,2,3,3,3,2,2,2,2,2,0,0,2,2,2,3,2,3,3,2,0,2,1,1,3,1,0,1,0,1,0,3,1,1,3,1,0,1,0,2,3,2,0,2,1,1,3,1,0,2,2,0,2,0,2,3,1,3,2,0,1,3,3,1,3,3,1,3,1,3,3,1,1,3,1,3,0,2,0,1,2,2,2,0,1,0,1,1,0,1,3,2,0,1,0,1,0,0,0,3,1,3,1,3,3,0,3,0,0,1]
      ]

-- test case for "tertius"
_test4 = findGraph 18 t
  where
    t = Trie.fromObservations $ zip plans results
    plans =
      [ "425143512242335431445525434543454501224200345111425055220200200004354125153524312322213052320444243520413051334450420234234313111512222202355053434011051144255450431432525022540555543420411121020510152340503152350323313352431020245452151151215310240303323200033535525354023013000521513150342523251014215523013024334541500033"
      , "452044354101215500234553012252550303410130425523110404441552341335223112423153551331232330254002321214544225303212404042052050533244120530022032511501345344530402410400110223044332441214234533123230024110331311302424505234003105223005035141414403052112150544323212131352405411010205344211330115504342204403205153155023125114"
      , "030200000101435501055450432333331335102352533303052122010412514251254023013001023210034511314235041302444030230404354032123332353430445143254034431005100424154415045443135333424005154300245533000344205222441024250021105530050323250515403521102022540000011012521504131355150341155433441522545050134212103205320535223112525401"
      ]
    results =
      [ [0,0,1,2,0,1,0,2,0,0,1,1,2,1,3,0,2,3,2,3,0,2,2,2,2,2,1,3,1,3,2,3,3,1,0,2,1,2,3,1,3,1,3,2,3,3,3,3,3,1,3,0,1,3,1,3,0,2,3,2,3,2,2,2,2,2,2,3,1,3,1,3,0,1,0,1,3,2,2,3,1,3,3,2,2,2,1,0,1,0,1,0,1,3,1,0,1,2,2,1,2,1,0,0,1,0,1,0,2,3,2,3,0,2,2,2,1,0,1,3,1,3,3,0,3,2,0,2,0,1,1,3,3,2,2,2,2,3,2,3,3,1,0,3,3,0,3,0,1,0,2,2,0,1,1,1,2,2,2,2,0,3,1,3,2,1,1,0,0,0,0,0,1,3,0,2,3,3,0,3,0,3,1,0,2,2,3,0,0,1,3,3,2,3,2,2,0,1,0,1,1,3,2,3,3,0,1,0,0,0,0,1,3,1,1,3,2,0,2,3,2,0,1,0,3,2,3,2,2,0,3,0,2,2,1,1,3,1,0,0,1,2,0,2,1,0,2,0,3,1,3,1,3,3,2,3,2,3,3,1,3,3,3,3,1,0,1,3,2,0,3,1,3,3,3,1,3,1,3,1,0,1,0,3,1,3,1,2,2,1,3,2,0,1,3,0,2,1,0,0,0,1,0,3,0,1,0,1,0,0,1,3,1,0,0,1,1,1,1,3,1,0,1,0,1]
      , [0,0,0,1,3,1,1,0,2,2,0,1,0,1,0,2,0,2,3,2,3,1,3,1,3,1,0,1,3,0,1,0,2,3,2,3,2,3,2,3,1,3,1,3,2,0,0,1,3,1,0,2,2,2,2,2,2,0,0,0,1,3,1,0,1,0,3,0,1,0,1,3,3,0,0,1,0,3,3,3,1,3,3,3,3,3,2,2,3,2,3,2,2,2,2,2,1,0,0,0,1,0,2,2,2,2,1,2,2,3,3,3,2,1,2,2,2,2,2,2,1,0,0,1,3,1,3,1,3,3,2,0,2,1,2,2,2,3,3,1,3,0,2,0,1,0,2,0,0,2,0,2,2,0,2,2,2,0,2,2,2,2,0,3,1,3,1,0,1,3,3,2,3,3,2,3,0,1,0,3,2,0,2,1,2,0,1,2,1,3,1,3,3,2,3,3,3,2,3,3,0,3,1,0,2,3,2,1,0,0,0,1,0,1,1,2,2,2,3,3,3,2,0,1,0,1,0,1,3,2,0,1,3,2,0,2,0,2,1,1,1,1,1,1,2,3,2,3,3,3,2,1,2,1,3,1,0,0,0,1,2,0,1,0,1,0,1,0,1,0,1,0,1,0,3,1,0,2,0,2,0,2,0,1,0,1,2,0,0,0,3,3,1,0,0,0,2,2,3,1,2,3,2,3,0,1,0,1,3,1,3,1,1,3,1,0,1,3,2,1,2,0,2,0,0]
      , [0,2,3,1,3,1,0,1,0,1,3,2,1,1,0,2,2,3,2,3,1,2,2,2,3,0,3,2,3,2,3,2,3,2,0,1,0,2,2,0,1,0,1,0,1,0,0,1,0,1,3,2,0,1,3,1,0,2,2,0,1,2,0,1,2,2,1,2,2,0,1,2,0,2,1,0,1,0,1,0,1,0,1,0,2,3,2,3,3,3,3,3,3,1,3,3,3,2,3,2,0,2,3,1,0,1,0,2,2,0,1,3,1,3,1,0,3,1,3,3,3,2,3,3,3,3,3,2,2,1,3,2,3,1,1,1,3,1,0,1,0,3,1,3,1,0,3,2,3,1,0,0,1,0,2,2,0,1,0,0,0,0,0,1,0,2,2,2,1,0,2,0,2,3,2,3,2,0,1,0,0,0,3,1,0,1,0,1,1,1,0,1,0,1,0,1,0,0,0,1,0,0,0,1,2,3,0,0,1,2,3,2,2,2,2,1,1,0,1,3,1,3,1,3,1,3,2,3,3,3,3,2,0,2,0,0,2,3,1,3,1,0,2,0,2,3,3,3,1,3,2,3,1,0,1,3,2,1,2,0,1,0,2,3,1,0,0,0,0,2,2,1,1,0,0,1,1,0,3,0,2,3,2,0,2,1,1,3,2,0,0,0,2,0,2,0,2,2,0,2,0,2,2,3,2,2,0,2,0,2,0,1,0,1,3,3,2,1,2,0,1,0,3,1,0]
      ]

_test_aleph = findGraph 12 t
  where
    t = Trie.fromObservations $ zip plans results
    plans =
      [ "315230050531253024030450101341005334135450131222554315430035103211234242"
      , "[0]3[1]1[2]5[3]2[0]3[1]0[2]0[3]5[0]0[1]5[2]3[3]1[0]2[1]5[2]3[3]0[0]2[1]4[2]0[3]3[0]0[1]4[2]5[3]0[0]1[1]0[2]1[3]3[0]4[1]1[2]0[3]0[0]5[1]3[2]3[3]4[0]1[1]3[2]5[3]4[0]5[1]0[2]1[3]3[0]1[1]2[2]2[3]2[0]5[1]5[2]4[3]3[0]1[1]5[2]4[3]3[0]0[1]0[2]3[3]5[0]1[1]0[2]3[3]2[0]1[1]1[2]2[3]3[0]4[1]2[2]4[3]2"
      ]
    results =
      [ [0,3,0,0,1,2,1,0,0,3,0,3,0,1,2,3,0,1,0,3,2,1,0,0,3,0,3,0,3,1,3,0,3,0,3,2,0,1,1,0,2,1,0,1,1,3,1,2,0,0,0,0,3,0,0,0,3,0,3,2,1,1,0,3,1,3,0,1,2,0,2,0,2]
      , [0,0,3,1,0,2,2,3,1,0,2,1,0,2,0,3,0,0,3,1,3,2,1,3,0,0,1,1,2,2,1,3,3,0,2,1,0,2,3,3,2,0,1,1,0,2,2,3,3,0,3,1,0,2,1,3,2,0,1,1,3,2,2,3,2,0,0,1,0,2,0,3,1,0,1,1,1,2,0,3,1,0,1,1,2,2,1,3,3,0,0,1,0,2,0,3,3,0,3,1,1,2,0,3,2,0,2,1,1,2,3,3,0,0,2,1,0,2,3,3,1,0,0,1,3,2,2,3,2,0,3,1,1,2,1,3,3,0,2,1,0,2,1,3,2]
      ]

_test_aleph_2 = findGraph 12 t
  where
    t = Trie.fromObservations $ zip plans results
    plans =
      [ "044201340430054134214055301210455205342053453552423024141112005304540004"
      , "0[0]4[1]4[2]2[3]0[0]1[1]3[2]4[3]0[0]4[1]3[2]0[3]0[0]5[1]4[2]1[3]3[0]4[1]2[2]1[3]4[0]0[1]5[2]5[3]3[0]0[1]1[2]2[3]1[0]0[1]4[2]5[3]5[0]2[1]0[2]5[3]3[0]4[1]2[2]0[3]5[0]3[1]4[2]5[3]3[0]5[1]5[2]2[3]4[0]2[1]3[2]0[3]2[0]4[1]1[2]4[3]1[0]1[1]1[2]2[3]0[0]0[1]5[2]3[3]0[0]4[1]5[2]4[3]0[0]0[1]0[2]4[3]"
      , "334415323241300232223122150511530500233545324104154245313224445421430000"
      , "3[0]3[1]4[2]4[3]1[0]5[1]3[2]2[3]3[0]2[1]4[2]1[3]3[0]0[1]0[2]2[3]3[0]2[1]2[2]2[3]3[0]1[1]2[2]2[3]1[0]5[1]0[2]5[3]1[0]1[1]5[2]3[3]0[0]5[1]0[2]0[3]2[0]3[1]3[2]5[3]4[0]5[1]3[2]2[3]4[0]1[1]0[2]4[3]1[0]5[1]4[2]2[3]4[0]5[1]3[2]1[3]3[0]2[1]2[2]4[3]4[0]4[1]5[2]4[3]2[0]1[1]4[2]3[3]0[0]0[1]0[2]0[3]"
      , "513312542432033412303212211322544504551255410555304153043312542400203451"
      , "5[0]1[1]3[2]3[3]1[0]2[1]5[2]4[3]2[0]4[1]3[2]2[3]0[0]3[1]3[2]4[3]1[0]2[1]3[2]0[3]3[0]2[1]1[2]2[3]2[0]1[1]1[2]3[3]2[0]2[1]5[2]4[3]4[0]5[1]0[2]4[3]5[0]5[1]1[2]2[3]5[0]5[1]4[2]1[3]0[0]5[1]5[2]5[3]3[0]0[1]4[2]1[3]5[0]3[1]0[2]4[3]3[0]3[1]1[2]2[3]5[0]4[1]2[2]4[3]0[0]0[1]2[2]0[3]3[0]4[1]5[2]1[3]"
      ]
    results =
      [ [0,0,0,1,2,1,1,3,3,3,3,1,0,2,1,0,1,0,1,2,1,0,2,1,1,0,0,1,2,1,0,1,2,1,2,1,2,0,0,0,2,1,3,3,1,3,1,1,2,3,2,0,2,0,1,1,0,1,1,3,2,1,0,2,0,2,3,1,0,0,2,1,0]
      , [0,0,0,0,1,1,2,2,3,1,0,1,1,3,2,2,3,3,0,0,1,1,2,0,3,2,0,2,1,0,2,0,3,2,0,3,1,0,2,1,3,3,0,2,1,1,2,2,3,1,0,0,1,2,2,1,3,3,0,0,1,0,2,3,3,1,0,3,1,2,2,1,3,0,0,0,1,0,2,3,3,0,0,3,1,1,2,2,3,1,0,0,1,2,2,3,3,2,0,3,1,1,2,1,3,1,0,2,1,1,2,1,3,3,0,3,1,0,2,3,3,0,0,3,1,3,2,2,3,2,0,0,1,2,2,1,3,2,0,3,1,0,2,3,3]
      , [0,1,3,3,3,3,1,3,2,0,1,0,1,3,3,3,2,0,1,2,0,1,3,2,0,1,2,1,2,1,1,1,0,0,0,2,1,2,0,0,2,3,1,3,2,3,3,3,3,3,1,0,0,0,2,0,1,3,2,0,1,0,0,2,3,2,1,0,0,0,2,1,0]
      , [0,1,0,3,1,1,2,2,3,3,0,1,1,3,2,2,3,0,0,1,1,0,2,0,3,0,0,2,1,0,2,2,3,0,0,1,1,3,2,0,3,1,0,2,1,2,2,3,3,1,0,3,1,0,2,1,3,1,0,0,1,0,2,0,3,0,0,2,1,3,2,2,3,2,0,0,1,3,2,2,3,1,0,3,1,1,2,0,3,2,0,0,1,0,2,2,3,3,0,1,1,2,2,1,3,2,0,3,1,3,2,1,3,1,0,3,1,3,2,3,3,1,0,2,1,1,2,0,3,2,0,3,1,0,2,0,3,2,0,0,1,2,2,3,3]
      , [0,2,1,0,1,3,2,1,0,0,0,1,2,1,0,1,0,1,2,0,2,0,1,1,2,0,1,1,3,2,0,2,3,3,1,0,0,2,1,3,2,1,1,0,1,0,0,0,0,0,0,0,1,2,0,2,3,1,3,3,2,1,0,0,0,0,2,0,0,0,1,2,1]
      , [0,2,0,1,1,0,2,1,3,3,0,0,1,3,2,2,3,0,0,3,1,2,2,1,3,1,0,0,1,1,2,1,3,0,0,2,1,0,2,1,3,2,0,0,1,2,2,3,3,1,0,1,1,2,2,0,3,3,0,0,1,0,2,3,3,3,0,2,1,0,2,1,3,2,0,1,1,0,2,0,3,1,0,1,1,0,2,2,3,2,0,2,1,0,2,1,3,3,0,3,1,3,2,1,3,3,0,2,1,0,2,3,3,0,0,3,1,1,2,2,3,3,0,0,1,1,2,1,3,1,0,3,1,2,2,2,3,2,0,3,1,3,2,1,3]
      ]
