{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverlappingInstances #-}

module HLogic(
    eq,
    notEq,
    (===),
    (=/=),
    run,
    Term(..),
    sth,
    success,
    fail,
    fresh,
    conso,
    membero,
    heado,
    tailo,
    emptyo,
    appendo,
    anyo,
    toTerm
) where

import Prelude hiding (fail)
import qualified Data.Map as Map
import Control.Monad hiding (fail)
import Data.List


data LVar = LVar Int deriving (Show, Eq, Ord)

data Term = TInt Int
          | TString String
          | TVar Int 
          | TNil
          | TAny
          | TCons Term Term deriving (Eq, Ord)

instance Show Term where
    show (TInt x) = show x
    show (TVar x) = "?_" ++ show x
    show TNil = "[]"
    show (TString s) = s
    show (TCons h t) = let
            showCons h (TCons a b) = show h ++ ", " ++ showCons a b
            showCons h TNil = show h ++ "]"
            showCons h v@(TVar _) = show h ++ ", " ++ show v ++ "...]"
        in "[" ++ showCons h t

class Termable a where
    toTerm :: a -> Term

instance Termable Int where
    toTerm = TInt

instance Termable LVar where
    toTerm (LVar a) = TVar a

instance Termable String where
    toTerm = TString

instance (Termable a) => Termable [a] where
    toTerm [] = TNil
    toTerm (x:xs) = TCons (toTerm x) (toTerm xs)

instance Termable Term where
    toTerm a = a

type Constrains = [(Term, Term)] -- disequality constrains
data Substitution = Substitution (Map.Map LVar Term) Int Constrains | Zonk deriving (Show, Eq)

newtype MLogic a = MLogic {runMLogic :: Substitution -> [(Substitution, a)]}
type Predicate = MLogic ()

emptySubstitution = Substitution Map.empty 0 []

sLookup :: LVar -> Substitution -> Maybe Term
sLookup a (Substitution s _ _) = Map.lookup a s

sInsert :: LVar -> Term -> Substitution -> Substitution
sInsert k v (Substitution s c d) = Substitution (Map.insert k v s) c d

--class Unify a b where
--  unify :: a -> b -> Substitution -> Substitution

unify :: Term -> Term -> Substitution -> Substitution
unify _ _ Zonk = Zonk
unify a b s = case (a, b) of
    (TInt a, TInt b) | a == b -> s
    (TString a, TString b) | a == b -> s
    (TCons ah at, TCons bh bt) -> unify at bt $ unify ah bh s
    (TNil, TNil) -> s
    (TVar a, TVar b) | a == b -> s
    (TVar v, a) -> case sLookup (LVar v) s of
        Nothing -> sInsert (LVar v) a s
        (Just val) -> unify val a s
    (a, v@(TVar _)) -> unify v a s
    (TAny, _) -> s
    (_, TAny) -> s
    _ -> Zonk

sth = TAny

verifyConstrain :: Substitution -> (Term, Term) -> Bool
verifyConstrain s (TVar a, TVar b) = deepLookup (LVar a) s /= deepLookup (LVar b) s
verifyConstrain s (TVar a, b) = deepLookup (LVar a) s /= b
verifyConstrain s (b, TVar a) = deepLookup (LVar a) s /= b
verifyConstrain s (a,b) = a /= b

verifyConstrains :: Substitution -> Bool
verifyConstrains s@(Substitution _ _ constrains) = all (verifyConstrain s) constrains

eq :: (Termable a, Termable b) => a -> b -> Predicate
eq a b = MLogic $ \s -> case unify (toTerm a) (toTerm b) s of
    s@(Substitution _ _ _) | verifyConstrains s -> [(s, ())]
    _ -> []

notEq :: (Termable a, Termable b) => a -> b -> Predicate
notEq a b = MLogic $ \s -> case unify (toTerm a) (toTerm b) s of
    Zonk -> [(s, ())]
    s' | s' == s -> []
    _ -> let (Substitution m c d) = s in [(Substitution m c ((toTerm a, toTerm b):d),  ())]

(===) :: (Termable a, Termable b) => a -> b -> Predicate
(===) = eq

(=/=) :: (Termable a, Termable b) => a -> b -> Predicate
(=/=) = notEq

success :: Predicate
success = return ()

fail :: Predicate
fail = mzero

getEnv :: MLogic Substitution
getEnv = MLogic $ \s -> [(s,s)]

instance Monad MLogic where
    a >>= f = MLogic $ \subst -> concatMap fn (runMLogic a subst)
        where
            fn (subst, x) = runMLogic (f x) subst
    return a = MLogic $ \s -> [(s, a)]

instance MonadPlus MLogic where
    mzero = MLogic $ \s -> []
    mplus a b = MLogic $ \s -> runMLogic a s ++ runMLogic b s

fresh :: MLogic Term
fresh = MLogic $ \(Substitution m counter d) -> [(Substitution m (counter + 1) d, TVar counter)]

conso :: (Termable a, Termable b, Termable c) => a -> b -> c -> Predicate
conso a b c = eq (TCons (toTerm a) (toTerm b)) (toTerm c)

membero :: (Termable a, Termable b) => a -> b -> Predicate
membero x xs = do
    h <- fresh
    t <- fresh
    conso h t xs
    (eq x h) `mplus` (membero x t)

heado a b = conso a sth b

tailo a b = conso sth a b

emptyo l = l === TNil

anyo g = mplus g (anyo g)

appendo l s out =
    ((emptyo l) >> (s === out))
    `mplus`
    (do
        a <- fresh
        d <- fresh
        r <- fresh
        conso a d l
        conso a r out
        appendo d s r)

deepLookup :: LVar -> Substitution -> Term
deepLookup var@(LVar v) sub = case sLookup var sub of
    Nothing -> TVar v
    (Just (TVar v)) -> deepLookup (LVar v) sub
    (Just x) -> x

reify :: Term -> Substitution -> Term
reify (TCons a b) sub = TCons (reify a sub) (reify b sub)
reify t@(TVar v) sub = case deepLookup (LVar v) sub of
    x | x == t -> t
    x -> reify x sub
reify x sub = x

run :: (Termable a) => MLogic a -> [Term]
run p = do
    (sub,res) <- runMLogic p emptySubstitution
    return $ reify (toTerm res) sub

