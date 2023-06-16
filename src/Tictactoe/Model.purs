module Tictactoe.Model where

import Relude

import Data.Number (pow)
import Tictactoe.Helpers (count)

data Symb = Empty | X | O
derive instance Eq Symb

rows :: Array (Array Int)
rows =
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

erdos :: Array Symb -> Number
erdos grid = sum $ rows <#> \row ->
  if row # any \i -> grid !! i == Just O then
    0.0
  else
    1.0 / pow 2.0 (toNumber (row # count \i -> grid !! i == Just Empty))


type Model =
  { grid :: Array Symb
  , erdosTable :: Maybe (Array Number)
  , history :: Array { square :: Int, symbol :: Symb, erdos :: Number }
  , locked :: Boolean
  }

init :: Model
init =
  { grid: replicate 25 Empty
  , history: []
  , erdosTable: Nothing
  , locked: false
  }

erdosTable :: Array Symb -> Array Number
erdosTable grid = grid # mapWithIndex \i symb ->
  if symb /= Empty then
    100.0
  else
    erdos $ updateAtIndices [i /\ O] grid


normalizeTable :: Array Number -> Array Number
normalizeTable table =
  let
    table' = table # filter \v -> v < 100.0
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