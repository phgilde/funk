module Inference.Infer (inferExpr, constraintsExpr, preludeEnv) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS as RWS
import CoreLanguage.CoreTypes (CoreScheme (..), CoreType (..), CoreExpr (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (nub)
import Debug.Trace (traceM)

type TypeEnv = Map.Map String CoreScheme

extend :: TypeEnv -> (String, CoreScheme) -> TypeEnv
extend env (k, v) = Map.insert k v env

remove :: TypeEnv -> String -> TypeEnv
remove env k = Map.delete k env
type Subst = Map.Map String CoreType
type Constraint = (CoreType, CoreType)

newtype InferState = InferState {count :: Int}
data TypeError
    = UnificationFail CoreType CoreType
    | InfiniteType String CoreType
    | UnboundVariable String
    | Ambigious [Constraint]
    | UnificationMismatch [CoreType] [CoreType] deriving (Show, Eq)

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

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `Set.union` ftv t2

letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: InferM CoreType
fresh = do
    s <- RWS.get
    RWS.put s{count = count s + 1}
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

generalize :: TypeEnv -> CoreType -> CoreScheme
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
        tv <- fresh
        (t0, constraints) <- listen . inEnv (n, Forall [] tv) $ infer e1
        traceM $ show (t0, constraints)
        traceM $ show tv
        subst <- lift . liftEither $ runSolve ((tv, t0):constraints)
        let t1 = apply subst t0
            sc = generalize env t1
        traceM $ show (sc, t1, apply subst tv)
        uni t1 (apply subst tv)
        inEnv (n, sc) (infer e2)

liftEither :: Either TypeError Subst -> ExceptT TypeError Identity Subst
liftEither ei = case ei of
    Left err -> throwE err
    Right subs -> return subs

type Unifier = (Subst, [Constraint])
type Solve a = ExceptT TypeError Identity a

s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1
unifies :: CoreType -> CoreType -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies t (TVar n) = n `bind` t
unifies (TVar n) t = n `bind` t
unifies (TArr a1 b1) (TArr a2 b2) = unifyMany [a1, b1] [a2, b2]
unifies (TCons n a) (TCons m b) | n == m = unifyMany a b
unifies t1 t2 = throwE $ UnificationFail t1 t2

unifyMany :: [CoreType] -> [CoreType] -> Solve Subst
unifyMany [] [] = return mempty
unifyMany (a:as) (b:bs) = do
    su1 <- unifies a b
    su2 <- unifyMany (apply su1 as) (apply su1 bs)
    return $ su2 `compose` su1


bind :: String -> CoreType -> Solve Subst
bind a t | t == TVar a = return mempty
        | occursCheck a t = throwE $ InfiniteType a t
        | otherwise = return (Map.singleton a t)

solver :: Unifier -> Solve Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2):rs) -> do
            su1 <- unifies t1 t2
            solver (su1 `compose` su, apply su1 rs)

runSolve cs = runIdentity $ runExceptT $ solver (mempty, cs)

inferTop :: TypeEnv -> [(String, CoreExpr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: CoreScheme -> CoreScheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TConstant _)    = []
    fv (TCons n a) = foldl1 (++) . map fv $ a

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCons n a) = TCons n (map normtype a)
    normtype (TConstant a)   = TConstant a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

initInfer = InferState { count = 0 }
-- | Run the inference monad
--runInfer :: TypeEnv -> InferM (CoreType, [Constraint]) -> Either TypeError (CoreType, [Constraint])
runInfer env (m :: InferM CoreType) = runExcept $ evalRWST m env initInfer

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: TypeEnv -> CoreExpr -> Either TypeError CoreScheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: TypeEnv -> CoreExpr -> Either TypeError ([Constraint], Subst, CoreType, CoreScheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ apply subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: CoreType -> CoreScheme
closeOver = normalize . generalize mempty

preludeEnv :: TypeEnv
preludeEnv = Map.fromList [("+", Forall [] $ TArr (TConstant "Int") (TArr (TConstant "Int") (TConstant "Int"))),
    ("-", Forall [] $ TArr (TConstant "Int") (TArr (TConstant "Int") (TConstant "Int"))),
    ("*", Forall [] $ TArr (TConstant "Int") (TArr (TConstant "Int") (TConstant "Int"))),
    ("/", Forall [] $ TArr (TConstant "Int") (TArr (TConstant "Int") (TConstant "Int")))]