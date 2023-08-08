import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS
import CoreLanguage.CoreTypes (CoreScheme (..), CoreType (..), CoreExpr (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Functor.Classes (eq1)

type TypeEnv = Map.Map String CoreScheme

extend env (k, v) = Map.insert k v env
remove env k = Map.delete k env
type Subst = Map.Map String CoreType
type Constraint = (CoreType, CoreType)

newtype InferState = InferState {count :: Int}
data TypeError
    = UnificationFail CoreType CoreType
    | InfiniteType String CoreType
    | UnboundVariable String
    | Ambigious [Constraint]
    | UnificationMismatch [CoreType] [CoreType]

type InferM a = (RWST TypeEnv [Constraint] InferState (Except TypeError)) a

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set String

instance Substitutable CoreType where
    apply _ (TConstant a) = TConstant a
    apply map t@(TVar n) = Map.findWithDefault t n map
    apply map (TArr a b) = TArr (apply map a) (apply map b)
    apply map (TCons n a) = TCons n (fmap (apply map) a)

    ftv TConstant{} = Set.empty
    ftv (TVar n) = Set.singleton n
    ftv (TArr a b) = ftv a <> ftv b
    ftv (TCons _ a) = foldr1 (<>) . fmap ftv $ a

instance Substitutable CoreScheme where
    apply map (Forall bound t) = Forall bound $ apply map' t
      where
        map' = foldr Map.delete map bound

    ftv (Forall bound t) = foldr Set.delete (ftv t) bound

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr1 (<>) . fmap ftv

instance Substitutable TypeEnv where
    apply map = Map.map (apply map)
    ftv = ftv . Map.elems

letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: InferM CoreType
fresh = do
    s <- get
    put s{count = count s + 1}
    return . TVar $ letters !! count s

occursCheck var t = var `Set.member` ftv t

uni :: CoreType -> CoreType -> InferM ()
uni a b = tell [(a, b)]

inEnv :: (String, CoreScheme) -> InferM a -> InferM a
inEnv (x, sc) m = do
    let scope e = remove e x `extend` (x, sc)
    local scope m
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ apply s t

generalize env t = Forall diff t
    where diff = Set.toList $ ftv t `Set.difference` ftv env
lookupEnv x = do
    env <- ask
    case Map.lookup x env of
        Nothing -> lift . throwE $ UnboundVariable x
        Just s -> instantiate s


infer expr = case expr of
    CeBool _ -> return $ TConstant "Bool"
    CeInt _ -> return $ TConstant "Int"

    CeVar x -> lookupEnv x
    CeAbs x e -> do
        tv <- fresh
        t <- inEnv (x, Forall [] tv) (infer e)
        return $ TArr tv t
    
    CeApp e1 e2 -> do
        t1 <- infer e1
        t2 <- infer e2
        tv <- fresh
        uni t1 (TArr t2 tv)
        return tv
    
    CeLet n e1 e2 -> do
        env <- ask
        t1 <- infer e1
        let sc = generalize env t1
        inEnv (n, sc) (infer e2)
        