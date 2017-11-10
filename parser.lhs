> module Parser where

An implementation of the Parser described in the the paper; "Monads for functional programming" by Philip Wadler.

> import Prelude hiding ((*))

The notation described in the paper clashes with multiplication from Prelude, hide it in this module.

> type ToParse = String
> type RemainingToParse = String
> type Parser a = ToParse -> [(a, RemainingToParse)]

> data Term = Con Int | Div Term Term

> item :: Parser Char
> item = \s -> item' s where
>                 item' [] = []
>                 item' (x:xs) = [(x, xs)]

> unit :: a -> Parser a
> unit a = \s -> [(a, s)]

> (*) :: Parser a -> (a -> Parser b) -> Parser b
> m * f = \s -> [(b, z) | (a, y) <- m s, (b, z) <- f a y]

> twoItems :: Parser (Char, Char)
> twoItems = item * (\a -> item * (\b -> unit (a, b)))

> threeItems :: Parser (Char, Char, Char)
> threeItems = item * (\a -> item * (\b -> item * (\c -> unit (a, b, c))))
