{-# LANGUAGE MultiWayIf #-}

module Lib where

import           Data.Char        (digitToInt)
import           Data.List        (foldl')
import           Text.Parsec
import           Text.Parsec.Char

parseList :: Parsec String () a -> Parsec String () [a]
parseList x = do
  char '['
  result <- x `sepBy` (char ',')
  char ']'
  pure result

ints :: Parsec String () [Integer]
ints = do
  char '['
  list <- integer `sepBy` (char ',')
  char ']'
  pure list

integer :: Parsec String () Integer
integer = do
  sign <- option '+' (oneOf "+-")
  absolute <- digitsToInt <$> many1 (fromIntegral . digitToInt <$> digit)
  if | sign == '+' -> pure absolute
     | sign == '-' -> pure (negate absolute)
     | otherwise   -> undefined

digitsToInt :: [Integer] -> Integer
digitsToInt = foldl' acc 0
  where
  acc s x = 10 * s + x

parse' :: Parsec String () a -> String -> Either ParseError a
parse' parser = parse parser "<repl>"
