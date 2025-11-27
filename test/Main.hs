module Main (main) where

import MicroKanren

import Control.Applicative (Alternative (..))
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import Data.Semigroup (sconcat)

assert :: Bool -> IO ()
assert True = pure ()
assert False = fail "Test failed"

satisfiable :: Goal a -> Bool
satisfiable = not . unsatisfiable

unsatisfiable :: Goal a -> Bool
unsatisfiable = null . run

main :: IO ()
main = do
  assert $ satisfiable (pure ())
  assert $ unsatisfiable empty

  -- Equality
  assert $ satisfiable $ 1 =:= 1
  assert $ unsatisfiable $ 1 =:= 2
  assert $ unsatisfiable $ 2 =:= 1

  -- Semigroup (conjunction)
  assert $ satisfiable $ pure () <> pure ()
  assert $ unsatisfiable $ pure () <> empty
  assert $ unsatisfiable $ empty <> pure ()
  assert $ unsatisfiable $ empty <> empty
  assert $ unsatisfiable $ 1 =:= 2 <> 1 =:= 1
  assert $ unsatisfiable $ 1 =:= 1 <> 2 =:= 1
  assert $ satisfiable $ 0 =:= 0 <> 5 =:= 5

  -- Functor
  assert $ eval (3 <$ fresh) == [Const 3]
  assert $ eval (pure <$> fresh) == [[Var 0]]
  -- Lazy in the LHS
  assert $ unsatisfiable (undefined <$> empty)

  -- Applicative (conjunction)
  assert $ eval (pure ()) == eval (pure ())
  assert $ eval (pure pure <*> pure ()) == [[()]]
  assert $ eval ((,) <$> pure () <*> pure ()) == [((), ())]
  assert $ eval ((,) <$> fresh <*> fresh) == [(Var 0, Var 1)]
  assert $ unsatisfiable ((+) <$> empty <*> fresh)
  -- Lazy in the RHS
  assert $ unsatisfiable ((,) <$> empty <*> undefined)

  -- Alternative (disjunction)
  assert $ run empty == run (empty $> ())
  assert $ satisfiable $ pure () <|> pure ()
  assert $ satisfiable $ pure () <|> empty
  assert $ satisfiable $ empty <|> pure ()
  assert $ unsatisfiable $ empty <|> empty

  -- Monadic (conjunction)
  let prog = do
        a <- fresh
        a =:= 9
        b <- fresh
        b =:= 8
        pure 10
  assert $ fmap subst (exec prog) == [[(1, 8), (0, 9)]]
  assert $ eval prog == [10]

  let prog = do
        a <- fresh
        (a =:= 9) <|> (a =:= 10)
        b <- fresh
        b =:= 8
        ask a
  assert $ fmap subst (exec prog) == [[(1, 8), (0, 9)], [(1, 8), (0, 10)]]
  assert $ eval prog == [Just 9, Just 10]

  -- Variable binding
  assert $ eval fresh == [Var 0]
  assert $ eval (fresh <|> fresh) == [Var 0, Var 0]
  assert $ eval (fresh <> fresh) == [Var 1]
  assert $ eval (sconcat $ fresh :| replicate 100 fresh) == [Var 100]
  assert $ satisfiable $ fresh >>= (=:= 1)

  -- Test a few unification shapes
  let prog = do
        a <- fresh
        b <- fresh
        c <- fresh
        a =:= c
        b =:= c
        c =:= 1
        catMaybes <$> traverse ask [b, c]
  assert $ eval prog == [Const <$> [1, 1]]

  let prog = do
        a <- fresh
        b <- fresh
        c <- fresh
        a =:= c
        b =:= c
        a =:= 1
        catMaybes <$> traverse ask [b, c]
  assert $ eval prog == [Const <$> [1, 1]]

  let prog = do
        a <- fresh
        b <- fresh
        c <- fresh
        d <- fresh
        a =:= b
        a =:= c
        b =:= d
        c =:= d
        b =:= 1
        catMaybes <$> traverse ask [a, b, c, d]
  assert $ eval prog == [Const <$> [1, 1, 1, 1]]

  let prog = do
        a <- fresh
        b <- fresh
        c <- fresh
        a =:= b
        b =:= c
        1 =:= a
        catMaybes <$> traverse ask [a, b, c]
  assert $ eval prog == [Const <$> [1, 1, 1]]

  let prog = do
        a <- fresh
        b <- fresh
        a =:= b
        b =:= a
        a =:= 2
        ask b
  assert $ eval prog == [Just (Const 2)]
