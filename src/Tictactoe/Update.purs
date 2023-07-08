module Tictactoe.Update (update) where

import Relude

import Control.Monad.Gen.Trans (GenState, runGen)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Pha.Update (Update, Milliseconds(..), delay)
import Tictactoe.Helpers (randomPick)
import Tictactoe.Model (Model, Symb(..), init, erdos, erdosTable, normalizeTable, bestMoves)
import Tictactoe.Msg (Msg(..))

type Env = { genState ∷ Ref GenState }

type Update' model msg a = Update model msg (ReaderT Env Aff) a

evalGen ∷ ∀ model msg a. Gen a → Update' model msg a
evalGen g = do
  { genState } ← ask
  st ← liftEffect $ Ref.read genState
  let v /\ st' = runGen g st
  liftEffect $ Ref.write st' genState
  pure v

update ∷ Msg → Update' Model Msg Unit
update (Play i) = do
  model <- get
  if model.locked || model.grid !! i /= Just Empty then
    pure unit
  else do
    let grid = model.grid # updateAtIndices [i /\ X]
    let erdosT = erdosTable grid
    let model' = model { grid = grid
                       , erdosTable = Just (normalizeTable erdosT)
                       , locked = true
                       , history = model.history `snoc` { square: i, symbol: X, erdos: erdos grid }
                       }
    put model' 
    if grid # all \s -> s /= Empty then
      pure unit
    else do
      delay $ Milliseconds 2000.0
      mj <- evalGen $ randomPick $ bestMoves erdosT
      case mj of
        Nothing -> pure unit 
        Just j -> do
          let grid' = grid # updateAtIndices [j /\ O]
          put $ model' { grid = grid'
                       , erdosTable = Nothing
                       , locked = false
                       , history = model'.history `snoc` { square: j, symbol: O, erdos: erdos grid' }
                       }

update Reinit = modify_ \model -> if model.locked then model else init