module Tictactoe.Update (update) where

import Relude

import Control.Monad.Gen.Trans (GenState, runGen)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Pha.Update (Update, Milliseconds(..), delay)
import Tictactoe.Helpers (randomPick)
import Tictactoe.Model (Model, Symb(..), Status(..), init, erdos, erdosTable, hasWon, normalizeTable, bestMoves)
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


showStatus ∷ Model → Update' Model Msg Unit
showStatus model = do
  put $ model {isStatusShown = true}
  delay $ Milliseconds 1000.0
  put $ model {isStatusShown = false}

update ∷ Msg → Update' Model Msg Unit
update (Play i) = do
  model <- get
  if model.locked || model.grid !! i /= Just Empty || model.status /= InProgress then
    pure unit
  else do
    let grid = model.grid # updateAtIndices [i /\ O]
    let erdosT = erdosTable grid
    let model' = model { grid = grid
                       , erdosTable = Just (normalizeTable erdosT)
                       , history = model.history `snoc` { square: i, symbol: X, erdos: erdos grid }
                       }
    if grid # all \s -> s /= Empty then
      put model'
    else if hasWon O grid then
      showStatus $ model' { status = HasWon }
    else do
      put model' { locked = true }
      delay $ Milliseconds 2000.0
      mj <- evalGen $ randomPick $ bestMoves erdosT
      case mj of
        Nothing -> pure unit 
        Just j -> do
          let grid' = grid # updateAtIndices [j /\ X]
          let erdosValue = erdos grid'
          let model'' = model' { grid = grid'
                               , erdosTable = Nothing
                               , locked = false
                               , history = model'.history `snoc` { square: j, symbol: O, erdos: erdosValue }
                               }
          if erdosValue == 0.0 then
            showStatus $ model'' { status = CannotWin }
          else if hasWon X grid' then
            showStatus $ model'' { status = HasLost }
          else
            put model''

update Reinit = modify_ \model -> if model.locked then model else init