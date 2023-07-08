module Tictactoe.View (view) where

import Relude

import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Events as E
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Tictactoe.Helpers (pairwise)
import Tictactoe.Model (Model, Symb(..))
import Tictactoe.Msg (Msg(..))

buttonClass ∷ String
buttonClass = "py-2.5 px-5 mr-2 mb-2 text-sm font-medium text-gray-900 focus:outline-none bg-white rounded-lg border border-gray-200 hover:bg-gray-100 hover:text-blue-700 focus:z-10 focus:ring-4 focus:ring-gray-200"

cardClass ∷ String
cardClass = "rounded overflow-hidden shadow-lg p-4"

card ∷ ∀ a. String → Array (Html a) → Html a
card title body =
  H.div [ H.class_ cardClass ]
    $ [ H.div [ H.class_ "font-bold text-xl mb-2" ] [ H.text title ] ]
    <> body

viewGrid ∷ Model → Html Msg
viewGrid {grid, erdosTable} =
  H.div
  [ H.class_ "relative shadow-md shadow-slate-200 m-4 select-none"
  , H.style "width" "300px"
  , H.style "height" "300px"
  ] $
    grid # mapWithIndex \i symb ->
      let
        row = i / 5
        col = i `mod` 5
        erdos = fromMaybe (-1.0) $ erdosTable >>= (_ !! i)
      in
        H.div
        [ H.class_ "absolute cursor-pointer flex items-center justify-center shadow text-4xl"
        , H.style "height" $ "20%" 
        , H.style "width" $ "20%"
        , H.style "left" $ show (toNumber col * 20.0) <> "%" 
        , H.style "top" $ show (toNumber row * 20.0) <> "%" 
        , H.style "background-color" $ if erdos == -1.0 then "white" else "rgb(255," <> show (255.0 * erdos) <> ",0)"
        , E.onClick \_ -> Play i
        ]
        [ case symb of 
            X -> H.span [H.style "color" "blue"] [H.text "X"]
            O -> H.span [H.style "color" "red"] [H.text "O"]
            Empty -> H.empty
        ]

viewErdos ∷ Array Number → Html Msg
viewErdos history = 
  H.div
    [ H.class_ "relative shadow-md shadow-slate-200 m-4 select-none"
    , H.style "width" "450px"
    , H.style "height" "300px" 
    ]
    [ S.svg [SA.viewBox (-5.0) (-5.0) 325.0 205.0]
      [ S.g [] $
          history # mapWithIndex \i erdos ->
            S.rect [ SA.x $ 10.0 * toNumber i - 2.0, SA.y $ 198.0 - 300.0 * erdos, SA.width 4.0, SA.height 4.0]
      , S.g [] $
          pairwise history # mapWithIndex \i (e1 /\ e2) ->
            S.line
              [ SA.x1 $ 10.0 * toNumber i
              , SA.x2 $ 10.0 * toNumber (i+1)
              , SA.y1 $ 200.0 - 300.0 * e1
              , SA.y2 $ 200.0 - 300.0 * e2
              , SA.strokeWidth 2.0
              , SA.stroke "black"
              ] 
      ]
    ]

view ∷ Model → Html Msg
view model =
  H.div [H.style "height" "400px", H.style "width" "850px"]
    [
      card "Tic-Tac-Toe" 
        [ H.div [H.class_ "flex flex-row"]
            [ H.div []
                [viewGrid model, H.button [H.class_ buttonClass, E.onClick \_ -> Reinit] [H.text "Recommencer"]]
            , H.div []
                [viewErdos (model.history <#> _.erdos)]
            ]
        ]
    ]
