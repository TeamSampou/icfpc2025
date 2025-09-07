module Graph.Z3
  ( findGraph
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
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Z3.Monad as Z3

import Base
import qualified Graph as Graph
import ObservationSummary (ObservationSummary, Trie)
import qualified ObservationSummary as Trie


findGraph :: Int -> Trie.ObservationSummary -> IO (Maybe (Graph.DiGraph, RoomIndex))
findGraph numRooms t = Z3.evalZ3 $ findGraph' numRooms t

findGraph' :: forall z3. Z3.MonadZ3 z3 => Int -> Trie.ObservationSummary -> z3 (Maybe (Graph.DiGraph, RoomIndex))
findGraph' numRooms t = do
  symRoom <- Z3.mkStringSymbol "Room"
  sRoom <- Z3.mkFiniteDomainSort symRoom (fromIntegral numRooms)
  rooms <- forM [0..numRooms-1] $ \i -> Z3.mkInt i sRoom
  let startingRoom = rooms !! 0

  doorFuncs <- forM [(0::Int)..5] $ \d -> do
    sym <- Z3.mkStringSymbol ("d" ++ show d)
    Z3.mkFuncDecl sym [sRoom] sRoom

  sLabel <- Z3.mkBvSort 2
  startingLabels <- forM [0..numRooms-1] $ \i -> do
    sym <- Z3.mkStringSymbol $ "starting_label_" ++ show i
    Z3.mkVar sym sLabel


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

  let f :: Seq Action -> Z3.AST -> [Z3.AST] -> Trie.ObservationSummary -> z3 ()
      f hist currentRoom currentLabels (Trie.Node label childrenD childrenL) = do
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


_test1 = findGraph 3 t
  where
    t = Trie.fromObservations $ zip plans results
    plans = ["000","123","213","333"] ++ ["4","5","02","03","04","05","31","32","34","35"]
    results = [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]] ++ [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]

_test2 = findGraph 6 t
  where
    t = Trie.fromObservation plan result
    plan = "021320403505044123550520034431312210134541025332505010033554343013423011254052531011533004340304253205132534"
    result = [0,0,3,0,0,3,3,3,3,2,3,3,1,2,3,3,0,3,2,3,1,2,3,2,0,0,0,2,3,2,0,0,0,0,0,0,0,0,0,2,3,3,0,1,1,1,0,0,3,1,2,3,3,0,1,1,1,0,0,0,2,1,1,1,1,1,0,2,1,1,1,1,2,1,1,1,2,3,2,3,2,0,1,2,0,0,0,0,1,1,3,2,3,3,2,0,2,1,1,0,3,3,1,1,0,3,1,1,1]

_test3 = findGraph 12 t
  where
    t = Trie.fromObservations $ zip plans results
    plans = ["205453044150242021134201232313140114532311403002015055315120434051505411210234055145550135005250420542531311152251205410534135504020204421105423154114445040134224053345420250520251533405521522155000534233441515505504","435131021401243542552323244440021455152551451033444503251113531545214043545145041444541553454340254001543412412352133131000251212054004522351501422005301411013322014531512252541532523315144543305302004142024312553022","014103511215141312305323252342322235214531434042525532110523445032451304013520554242433313050232041103421223212411115241323323432034111153443051150454203404154552033513314320100420523050434415242135525503142022053521"]
    results = [[0,1,0,2,0,1,3,3,1,1,2,0,1,0,3,1,3,1,1,0,0,0,0,1,2,1,3,3,2,3,3,1,3,2,3,2,0,1,3,1,3,1,1,1,1,3,2,3,1,3,1,0,2,0,2,3,3,2,3,1,3,2,3,2,1,0,1,0,2,0,0,0,1,0,1,1,1,3,2,3,1,0,1,2,0,1,0,2,2,2,0,2,1,0,1,0,2,2,1,1,0,0,0,2,3,3,2,3,2,2,0,1,1,0,1,0,2,0,2,2,3,0,3,1,0,0,1,0,2,2,3,1,3,1,3,2,0,0,0,1,1,0,0,0,0,0,1,3,2,2,0,3,2,0,1,2,1,2,2,2,1,0,3,3,0,3,3,2,0,2,1,1,1,0,2,0,1,0,1,0,0,1,3,0,3,3,0,1,1,1,0,0,1,0,2,0,1,1,0,2,3,2,3,2,3,2,2,2,0,1,0,1,0,2,0,1,2],[0,2,2,0,0,2,2,3,1,1,1,1,0,1,1,3,2,0,0,1,0,0,0,0,0,0,2,0,3,2,3,3,1,1,1,0,2,3,0,0,2,0,1,1,0,1,0,0,3,1,1,2,0,2,2,3,2,3,2,2,2,0,3,2,0,2,0,1,0,2,3,1,3,0,0,1,0,2,0,1,2,2,2,0,3,1,3,2,0,2,3,2,0,0,3,1,0,1,0,0,2,1,2,0,2,2,2,2,1,2,2,3,2,0,1,0,0,3,2,2,2,1,1,0,1,0,0,0,0,0,2,0,0,2,1,2,0,0,1,3,0,0,1,3,1,3,1,3,3,2,0,0,2,2,2,2,2,3,2,2,2,3,3,1,2,0,1,3,1,0,1,0,0,1,1,0,3,1,0,2,1,0,1,3,3,1,0,1,2,2,0,2,2,2,1,0,0,2,3,1,1,1,2,0,0,2,3,2,2,2,3,2,0,0,2,3,3],[0,1,2,0,1,1,3,0,0,1,0,1,0,1,1,2,2,2,3,2,3,1,3,1,3,1,0,0,3,1,1,3,1,3,1,3,1,3,1,3,0,3,2,0,2,0,2,0,0,1,1,0,1,3,1,1,0,1,0,1,3,1,1,0,1,3,3,2,0,0,2,3,1,0,1,3,0,0,1,0,2,0,0,0,0,2,2,2,2,2,2,1,0,1,0,2,1,1,1,2,2,3,3,2,3,3,3,3,2,3,3,3,2,2,2,2,2,0,0,2,2,2,3,2,3,3,2,0,2,1,1,3,1,0,1,0,1,0,3,1,1,3,1,0,1,0,2,3,2,0,2,1,1,3,1,0,2,2,0,2,0,2,3,1,3,2,0,1,3,3,1,3,3,1,3,1,3,3,1,1,3,1,3,0,2,0,1,2,2,2,0,1,0,1,1,0,1,3,2,0,1,0,1,0,0,0,3,1,3,1,3,3,0,3,0,0,1]]
