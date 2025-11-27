{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module MicroKanren
  ( run
  , eval
  , exec
  , (=:=)
  , Goal
  , Term (..)
  , State (subst)
  , fresh
  , ask
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Data.Maybe (maybeToList)
import Data.Tuple (swap)

type Var = Int
type Subst = [(Var, Term)]

data State
  = State
  { subst :: Subst
  , freshVar :: Var
  }
  deriving (Eq, Show)

data Term
  = Var Int
  | Const Int
  | Cons Term Term
  deriving (Eq, Show)

instance Num Term where
  fromInteger = Const . fromInteger

-- We have to newtype this, in order to not inherit the non-interleaved disjunction
-- that we would get for free from the Semigroup instance for functions
newtype Goal a = Goal {unGoal :: State -> [(a, State)]}

run :: Goal a -> [(a, State)]
run = ($ State{subst = [], freshVar = 0}) . unGoal

exec :: Goal a -> [State]
exec = fmap snd . run

eval :: Goal a -> [a]
eval = fmap fst . run

(=:=) :: Term -> Term -> Goal ()
(=:=) a b = Goal $ \State{..} -> maybeToList $ wrap freshVar <$> unify a b subst
 where
  wrap freshVar subst = ((), State{..})

fresh :: Goal Term
fresh = Goal $ \State{..} -> [(Var freshVar, State{freshVar = succ freshVar, subst})]

walk :: Var -> Subst -> Maybe Term
walk a subst =
  lookup a subst >>= \case
    Var c -> walk c subst <|> Just (Var c)
    c -> Just c

var :: Term -> Maybe Var
var (Var a) = Just a
var _ = Nothing

unify :: Term -> Term -> Subst -> Maybe Subst
unify a b subst = case (a, b) of
  (Var a, Var b) | a == b -> pure subst
  (Var a, _) -> case walk a subst of
    Nothing -> case var b >>= flip walk subst of
      Nothing -> pure $ (a, b) : subst
      Just c -> unify (Var a) c subst
    Just c -> unify c b subst
  (_, Var _) -> unify b a subst
  (Const n, Const m) | n == m -> pure subst
  (Cons a b, Cons c d) -> do
    subst <- unify a c subst
    unify b d subst
  _ -> Nothing

ask :: Term -> Goal (Maybe Term)
ask (Var a) = Goal $ \state@State{..} -> [(walk a subst, state)]
ask _ = pure Nothing

-- Helpers
----------

interleave :: [a] -> [a] -> [a]
interleave (x : xs) (y : ys) = x : y : interleave xs ys
interleave xs ys = xs <> ys

-- Instances
------------

instance Semigroup (Goal a) where
  f <> g = Goal $ \state -> fmap snd (unGoal f state) >>= unGoal g

instance Functor Goal where
  fmap f goal = Goal $ \state -> first f <$> unGoal goal state

instance Applicative Goal where
  f <*> g = Goal $ \state -> do
    (f, newState) <- unGoal f state
    fmap (first f) $ unGoal g newState
  pure a = Goal $ \state -> [(a, state)]

instance Monad Goal where
  f >>= g = Goal $ \state -> do
    (a, newState) <- unGoal f state
    unGoal (g a) newState

instance Alternative Goal where
  empty = Goal $ pure []
  f <|> g = Goal $ \state -> interleave (unGoal f state) (unGoal g state)
