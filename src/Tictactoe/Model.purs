module Tictactoe.Model where

import Relude

import Data.Array (drop)
import Data.Number (pow)
import Tictactoe.Config (rules, Rules(..))
import Tictactoe.Helpers (count)

data Symb = Empty | X | O
derive instance Eq Symb

data Status = InProgress | HasWon | HasLost | CannotWin
derive instance Eq Status

rows5 :: Array (Array Int)
rows5 =
  [ [0, 1, 2, 3, 4]
  , [5, 6, 7, 8, 9]
  , [10, 11, 12, 13, 14]
  , [15, 16, 17, 18, 19]
  , [20, 21, 22, 23, 24]
  , [0, 5, 10, 15, 20]
  , [1, 6, 11, 16, 21]
  , [2, 7, 12, 17, 22]
  , [3, 8, 13, 18, 23]
  , [4, 9, 14, 19, 24]
  , [0, 6, 12, 18, 24]
  , [4, 8, 12, 16, 20]
  ]

rows4 :: Array (Array Int)
rows4 = (rows5 >>= \row -> [take 4 row, drop 1 row]) <>
  [ [1, 7, 13, 19]
  , [3, 7, 11, 15]
  , [5, 11, 17, 23]
  , [9, 13, 17, 21]
  ]

rows :: Array (Array Int)
rows = case rules of
  Rules4 -> rows4
  Rules5 -> rows5

erdos :: Array Symb -> Number
erdos grid = sum $ rows <#> \row ->
  if row # any \i -> grid !! i == Just X then
    0.0
  else
    1.0 / pow 2.0 (toNumber (row # count \i -> grid !! i == Just Empty))


type Model =
  { grid :: Array Symb
  , erdosTable :: Maybe (Array Number)
  , history :: Array { square :: Int, symbol :: Symb, erdos :: Number }
  , status :: Status
  , locked :: Boolean
  }

init :: Model
init =
  { grid: replicate 25 Empty
  , history: []
  , erdosTable: Nothing
  , locked: false
  , status: InProgress
  }

erdosTable :: Array Symb -> Array Number
erdosTable grid = grid # mapWithIndex \i symb ->
  if symb /= Empty then
    100.0
  else
    erdos $ updateAtIndices [i /\ X] grid

hasWon :: Symb -> Array Symb -> Boolean
hasWon who grid =
  rows # any \row ->
    row # all \idx -> grid !! idx == Just who

normalizeTable :: Array Number -> Array Number
normalizeTable table =
  let
    table' = table # filter (_ < 100.0)
    min = fromMaybe 0.0 $ minimum table'
    max = fromMaybe 0.0 $ maximum table'
  in
    table <#> \v ->
      if v == 100.0 then
        -1.0
      else if min == max then
        1.0
      else
        (v - min) / (max - min)

bestMoves :: Array Number -> Array Int
bestMoves table =
  let min = fromMaybe 0.0 $ minimum table
  in
    table
    # mapWithIndex (\i s -> (i /\ s))
    # filter (\t -> snd t == min)
    # map fst