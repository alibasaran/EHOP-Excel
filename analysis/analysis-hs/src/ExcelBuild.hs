{-# LANGUAGE ConstraintKinds, RankNTypes, FlexibleContexts, ScopedTypeVariables, TupleSections #-}

module ExcelBuild where

{-
    A single file containing __only__ the Excel build code 
    from https://github.com/snowleopard/build
-}

import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)

import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Algebra.Graph
import qualified Algebra.Graph.ToGraph as T
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Maybe
import Data.Bool
import Data.Char
import Data.String
import qualified Text.Read as Read

----------------------------------- Store ----------------------------------
-- | A 'Hash' is used for efficient tracking and sharing of build results. We
-- use @newtype Hash a = Hash a@ for prototyping.
newtype Hash a = Hash a deriving (Eq, Ord,Show)

instance Functor Hash where
    fmap f (Hash a) = Hash (f a)

instance Applicative Hash where
    pure = Hash
    Hash f <*> Hash a = Hash (f a)

class Ord a => Hashable a where
    -- | Compute the hash of a given value. We typically assume cryptographic
    -- hashing, e.g. SHA256.
    hash :: a -> Hash a

instance Hashable Int where
    hash = Hash

instance Hashable Integer where
    hash = Hash

instance Hashable a => Hashable [a] where
    hash = Hash

instance Hashable a => Hashable (Hash a) where
    hash = Hash

instance (Hashable a, Hashable b) => Hashable (a, b) where
    hash = Hash

-- | An abstract datatype for a key/value store with build information of type @i@.
data Store i k v = Store { info :: i, values :: k -> v }

-- | Read the build information.
getInfo :: Store i k v -> i
getInfo = info

-- | Read the value of a key.
getValue :: k -> Store i k v -> v
getValue = flip values

-- | Read the hash of a key's value. In some cases may be implemented more
-- efficiently than @hash . getValue k@.
getHash :: Hashable v => k -> Store i k v -> Hash v
getHash k = hash . getValue k

-- | Write the build information.
putInfo :: i -> Store i k v -> Store i k v
putInfo i s = s { info = i }

-- | Modify the build information.
mapInfo :: (i -> j) -> Store i k v -> Store j k v
mapInfo f (Store i kv) = Store (f i) kv

-- | Update the value of a key.
putValue :: Eq k => k -> v -> Store i k v -> Store i k v
putValue k v s = s { values = \key -> if key == k then v else values s key }

-- | Initialise the store.
initialise :: i -> (k -> v) -> Store i k v
initialise = Store

----------------------------------- Tasks ----------------------------------

-- | A 'Task' is used to compute a value of type @v@, by finding the necessary
-- dependencies using the provided @fetch :: k -> f v@ callback.
newtype Task c k v = Task { run :: forall f. c f => (k -> f v) -> f v }

-- | 'Tasks' associates a 'Task' with every non-input key. @Nothing@ indicates
-- that the key is an input.
type Tasks c k v = k -> Maybe (Task c k v)

-- | Compose two task descriptions, preferring the first one in case there are
-- two tasks corresponding to the same key.
compose :: Tasks Monad k v -> Tasks Monad k v -> Tasks Monad k v
compose t1 t2 key = t1 key <|> t2 key

-- | Lift an applicative task to @Task Monad@. Use this function when applying
-- monadic task combinators to applicative tasks.
liftTask :: Task Applicative k v -> Task Monad k v
liftTask (Task task) = Task task

-- | Lift a collection of applicative tasks to @Tasks Monad@. Use this function
-- when building applicative tasks with a monadic build system.
liftTasks :: Tasks Applicative k v -> Tasks Monad k v
liftTasks = fmap (fmap liftTask)

----------------------------------- Dirty bit rebuilder ----------------------------------
-- | Given a key-value pair and the corresponding task, a rebuilder returns a
-- new task that has access to the build information and can use it to skip
-- rebuilding a key if it is up to date.
type Rebuilder c i k v = k -> v -> Task c k v -> Task (MonadState i) k v
-- | If the key is dirty, rebuild it. Used by Excel.
dirtyBitRebuilder :: Rebuilder Monad (k -> Bool) k v
dirtyBitRebuilder key value task = Task $ \fetch -> do
    isDirty <- get
    if isDirty key then run task fetch else return value

-- | If the key is dirty, rebuild it and clear the dirty bit. Used by Excel.
dirtyBitRebuilderWithCleanUp :: Ord k => Rebuilder Monad (Set k) k v
dirtyBitRebuilderWithCleanUp key value task = Task $ \fetch -> do
    isDirty <- get
    if key `Set.notMember` isDirty then return value else do
        put (Set.delete key isDirty)
        run task fetch


---------------------------------- Build ----------------------------------
-- | Build a dependency graph given a function for computing dependencies of a
-- key and a target key.
graph :: Ord k => (k -> [k]) -> k -> Graph k
graph deps key = transpose $ overlays [ star k (deps k) | k <- keys Set.empty [key] ]
  where
    keys seen []   = Set.toList seen
    keys seen (x:xs)
        | x `Set.member` seen = keys seen xs
        | otherwise           = keys (Set.insert x seen) (deps x ++ xs)

-- | Execute a monadic task on a pure store @k -> v@, tracking the dependencies.
trackPure :: Task Monad k v -> (k -> v) -> (v, [k])
trackPure task fetch = runWriter $ run task (\k -> writer (fetch k, [k]))

-- | Run a task in a given store.
compute :: Task Monad k v -> Store i k v -> v
compute task store = runIdentity $ run task (\k -> Identity (getValue k store))

-- | Compute all keys reachable via dependecies from a target key.
reachable :: Ord k => (k -> [k]) -> k -> [k]
reachable deps key = vertexList (graph deps key)

-- | A build system takes a description of 'Tasks', a target key, and a store,
-- and computes a new store, where the key and its dependencies are up to date.
type Build c i k v = Tasks c k v -> k -> Store i k v -> Store i k v

-- | Given a description of @tasks@, an initial @store@, and a @result@ produced
-- by running a build system on a target @key@, this function returns 'True' if
-- the @result@ is a correct build outcome. Specifically:
-- * @result@ and @store@ must agree on the values of all inputs. In other words,
--   no inputs were corrupted during the build.
-- * @result@ is /consistent/ with the @tasks@, i.e. for every non-input key,
--   the result of recomputing its task matches the value stored in the @result@.
correctBuild :: (Ord k, Eq v) => Tasks Monad k v -> Store i k v -> Store i k v -> k -> Bool
correctBuild tasks store result = all correct . reachable deps
  where
    deps = maybe [] (\task -> snd $ trackPure task (`getValue` result)) . tasks
    correct k = case tasks k of
        Nothing   -> getValue k result == getValue k store
        Just task -> getValue k result == compute task result

---------------------------------- Restarting scheduler ----------------------------------
-- | Convert a task with a total lookup function @k -> m v@ into a task
-- with a lookup function that can throw exceptions @k -> m (Either e v)@. This
-- essentially lifts the task from the type of values @v@ to @Either e v@,
-- where the result @Left e@ indicates that the task failed, e.g. because of a
-- failed dependency lookup, and @Right v@ yeilds the value otherwise.
type Scheduler c i j k v = Rebuilder c j k v -> Build c i k v

-- | Lift a computation operating on @i@ to @Store i k v@.
liftStore :: State i a -> State (Store i k v) a
liftStore x = do
    (a, newInfo) <- gets (runState x . getInfo)
    modify (putInfo newInfo)
    return a

-- | Lift a computation operating on @Store i k v@ to @Store (i, j) k v@.
liftInfo :: State (Store i k v) a -> State (Store (i, j) k v) a
liftInfo x = do
    store <- get
    let (a, newStore) = runState x (mapInfo fst store)
    put $ mapInfo (, snd $ getInfo store) newStore
    return a

updateValue :: Eq k => k -> v -> v -> Store i k v -> Store i k v
updateValue key _current_value = putValue key

try :: Task (MonadState i) k v -> Task (MonadState i) k (Either e v)
try task = Task $ \fetch -> runExceptT $ run task (ExceptT . fetch)

-- | The so-called @calculation chain@: the order in which keys were built
-- during the previous build, which is used as the best guess for the current
-- build by Excel and other similar build systems.
type Chain k = [k]

-- | A model of the scheduler used by Excel, which builds keys in the order used
-- in the previous build. If a key cannot be build because its dependencies have
-- changed and a new dependency is still dirty, the corresponding build task is
-- abandoned and the key is moved at the end of the calculation chain, so it can
-- be restarted when all its dependencies are up to date.
restarting :: forall ir k v. Ord k => Scheduler Monad (ir, Chain k) ir k v
restarting rebuilder tasks target = execState $ do
    chain    <- gets (snd . getInfo)
    newChain <- liftInfo $ go Set.empty $ chain ++ [target | target `notElem` chain]
    modify . mapInfo $ \(ir, _) -> (ir, newChain)
  where
    go :: Set k -> Chain k -> State (Store ir k v) (Chain k)
    go _    []       = return []
    go done (key:ks) = case tasks key of
        Nothing -> (key :) <$> go (Set.insert key done) ks
        Just task -> do
            store <- get
            let value = getValue key store
                newTask :: Task (MonadState ir) k (Either k v)
                newTask = try $ rebuilder key value task
                fetch :: k -> State ir (Either k v)
                fetch k | k `Set.member` done = return $ Right (getValue k store)
                        | otherwise           = return $ Left k
            result <- liftStore (run newTask fetch)
            case result of
                Left dep -> go done $ dep : filter (/= dep) ks ++ [key]
                Right newValue -> do
                    modify $ updateValue key value newValue
                    (key :) <$> go (Set.insert key done) ks

---------------------------------- Excel ----------------------------------

-- | Excel stores a dirty bit per key and a calc chain.
type ExcelInfo k = (k -> Bool, Chain k)

-- | A model of Excel: a monadic build system that stores the calculation chain
-- from the previuos build and approximate dependencies.
excel :: Ord k => Build Monad (ExcelInfo k) k v
excel = restarting dirtyBitRebuilder

---------------------------------- Spreadsheet ----------------------------------

------------ Cell -------------
-- | A 'Cell' is described by a pair integers: 'row' and 'column'. We provide
-- @IsString@ instance for convenience, so @"A8"@ corresponds to @Cell 8 0@.
data Cell = Cell { row :: Int, column :: Int } deriving (Eq, Ord)

-- | Get the name of a 'Cell', e.g. @name (Cell 8 0) == "A8"@.
name :: Cell -> String
name (Cell r c) | c >= 0 && c < 26 = chr (c + ord 'A') : show r
                | otherwise        = show (Cell r c)

instance IsString Cell where
    fromString string = case string of
        columnChar : rowIndex -> Cell r c
            where
              r = fromMaybe fail (Read.readMaybe rowIndex)
              c | isAsciiUpper columnChar = ord columnChar - ord 'A'
                | otherwise               = fail
        _ -> fail
      where
        fail = error $ "Cannot parse cell name " ++ string

instance Show Cell where
    show = name

instance Hashable Cell where
    hash (Cell row column) = Cell <$> hash row <*> hash column

------------ Spreadsheet itself -------------
-- | Some cells contain formulas for computing values from other cells. 
data Formula = Constant Int
             | Reference Cell
             | RelativeReference Int Int
             | Unary (Int -> Int) Formula
             | Binary (Int -> Int -> Int) Formula Formula
             | IfZero Formula Formula Formula
             | Random Int Int

instance Num Formula where
    fromInteger = Constant . fromInteger
    (+)    = Binary (+)
    (-)    = Binary (-)
    (*)    = Binary (*)
    abs    = Unary abs
    signum = Unary signum

instance IsString Formula where
    fromString = Reference . fromString

-- | A short alias for 'RelativeReference'.
rel :: Int -> Int -> Formula
rel = RelativeReference

-- | A spreadsheet is a partial mapping of cells to formulas. Cells for which
-- the mapping returns @Nothing@ are inputs.
type Spreadsheet = Cell -> Maybe Formula

-- | Monadic spreadsheet computation.
spreadsheetTask :: Spreadsheet -> Tasks Monad Cell Int
spreadsheetTask spreadsheet cell@(Cell r c) = case spreadsheet cell of
    Nothing      -> Nothing -- This is an input
    Just formula -> Just $ Task $ evaluate formula
  where
    evaluate formula fetch = go formula
      where go formula = case formula of
                Constant x              -> pure x
                Reference cell          -> fetch cell
                RelativeReference dr dc -> fetch (Cell (r + dr) (c + dc))
                Unary  op fx            -> op <$> go fx
                Binary op fx fy         -> op <$> go fx <*> go fy
                IfZero fx fy fz         -> do
                    x <- go fx
                    if x == 0 then go fy else go fz
                Random _ _      -> error "Not supported by monadic tasks"

-- | A build system that acceptes a list of target keys.
-- More similar to how spreadsheets are rebuilt in the Koka application
type MultiBuild c i k v = Tasks c k v -> [k] -> Store i k v -> Store i k v

sequentialMultiBuild :: Build Monad i k v -> MultiBuild Monad i k v
sequentialMultiBuild build task outputs store = case outputs of
    []     -> store
    (k:ks) -> sequentialMultiBuild build task ks (build task k store)