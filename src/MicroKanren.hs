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
  , unit
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)
import Data.Maybe (fromMaybe, maybeToList)

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
  | Unit
  | Cons Term Term
  deriving Eq

instance Num Term where
  fromInteger 0 = Unit
  fromInteger n = Cons Unit $ fromInteger $ n - 1

-- We have to newtype this, in order to not inherit the non-interleaved disjunction
-- that we would get for free from the Semigroup instance for functions
newtype Goal a = Goal {unGoal :: State -> [(a, State)]}

run :: Goal a -> [(a, State)]
run goal = unGoal goal $ State{subst = [], freshVar = 0}

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

walk :: Term -> Subst -> Term
walk Unit _ = Unit
walk (Cons a b) subst = Cons (walk a subst) (walk b subst)
walk (Var a) subst =
  fromMaybe (Var a) $
    lookup a subst >>= \case
      Var c -> Just $ walk (Var c) subst
      c -> Just c

unify :: Term -> Term -> Subst -> Maybe Subst
unify a b subst = case (walk a subst, walk b subst) of
  (Unit, Unit) -> pure subst
  (Cons a b, Cons c d) -> do
    subst <- unify a c subst
    unify b d subst
  (Var a', Var b') | a' == b' -> pure subst
  (Var a', b')
    | a == Var a' -> pure $ (a', b') : subst
    | otherwise -> unify (Var a') b' subst
  (_, Var _) -> unify b a subst
  _ -> Nothing

unit :: Goal Term
unit = pure Unit

ask :: Term -> Goal Term
ask (Var a) = Goal $ \state@State{..} -> [(walk (Var a) subst, state)]
ask a = pure a

-- Helpers
----------

interleave :: [a] -> [a] -> [a]
interleave (x : xs) (y : ys) = x : y : interleave xs ys
interleave xs ys = xs <> ys

toNumber :: Term -> Maybe Int
toNumber (Cons Unit a) = succ <$> toNumber a
toNumber Unit = Just 0
toNumber _ = Nothing

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

instance Show Term where
  show t = case (toNumber t, t) of
    (Just a, _) -> show a
    (_, Var a) -> "Var " <> show a
    (_, Cons a b) -> "cons (" <> show a <> ") (" <> show b <> ")"
