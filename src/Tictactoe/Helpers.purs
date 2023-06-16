module Tictactoe.Helpers where

import Relude

import Control.Monad.Gen.Trans (chooseInt)

randomPick ∷ ∀ a. Array a → Gen (Maybe a)
randomPick [] = pure Nothing
randomPick t = (t !! _) <$> chooseInt 0 (length t - 1)

count ∷ ∀ a. (a → Boolean) → Array a → Int
count f = length <<< filter f