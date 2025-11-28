module Language.MicroKanren.Stdlib
  ( succ
  , add
  ) where

import Language.MicroKanren

import Control.Applicative

import Prelude hiding (succ)

succ :: Term -> Term -> Goal ()
succ a b = a =:= Cons Unit b

add :: Term -> Term -> Term -> Goal ()
add res a b = do
  base <|> other
 where
  base = a =:= 0 <> res =:= b
  other = do
    apred <- fresh
    bsucc <- fresh
    succ a apred
    succ bsucc b
    add res apred bsucc
