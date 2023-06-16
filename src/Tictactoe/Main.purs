module Tictactoe.Main
  where

import Relude hiding (view)

import Control.Monad.Reader.Trans (runReaderT)
import Effect.Ref as Ref
import Pha.App (app)
import Pha.Update (hoist)
import Random.LCG (randomSeed)
import Tictactoe.View (view)
import Tictactoe.Model (init)
import Tictactoe.Update (update)

main ∷ Effect Unit
main = do
  newSeed ← randomSeed
  genState ← Ref.new { newSeed, size: 0 }
  app
    { init: { model: init, msg: Nothing }
    , view
    , update: hoist (flip runReaderT { genState }) <<< update
    , selector: "#root"
    }
