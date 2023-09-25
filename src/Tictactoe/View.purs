module Tictactoe.View (view) where

import Relude
import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Events as E
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Tictactoe.Helpers (pairwise)
import Tictactoe.Config (rules, Rules(..))
import Tictactoe.Model (Model, Symb(..), Status(..))
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
            X -> H.span [H.style "color" "rgb(208,0,208)"] [H.text "X"]
            O -> H.span [H.style "color" "rgb(0,199,199)"] [H.text "O"]
            Empty -> H.empty
        ]


scaleY :: Number
scaleY = case rules of
  Rules4 -> 75.0 
  Rules5 -> 300.0

viewErdos ∷ Array Number → Html Msg
viewErdos history =
  H.div
    [ H.class_ "relative shadow-md shadow-slate-200 m-4 select-none"
    , H.style "width" "450px"
    , H.style "height" "320px" 
    ]
    [ S.svg [SA.viewBox (-15.0) (-30.0) 340.0 235.0]
      [ S.text
          [SA.x (-15), SA.y (-15), SA.stroke "black", SA.fontSize 12]
          [H.text "Danger"]
      , S.line [SA.x1 0, SA.x2 0, SA.y1 0, SA.y2 200, SA.stroke "black"]
      , S.g [] $
        (0 .. 10) <#> \i ->
          S.line [SA.x1 (-3), SA.x2 3, SA.y1 $ i * 20, SA.y2 $ i * 20, SA.stroke "black"]
      , S.g [] $
        (0 .. 10) <#> \i ->
          S.text
            [SA.x (-15), SA.y $ i * 20, SA.stroke "black", SA.fontSize 10]
            [H.text $ if i == 0 then "1.0" else "0." <> show (10-i)]
      , S.line [SA.x1 0, SA.x2 300, SA.y1 200, SA.y2 200, SA.stroke "black"]
      , S.g [] $
        (0 .. 25) <#> \i ->
          S.line [SA.y1 197, SA.y2 203, SA.x1 $ i * 10, SA.x2 $ i * 10, SA.stroke "black"]
      , S.g [] $
          history # mapWithIndex \i erdos ->
            S.rect [ SA.x $ 10 * (i+1) - 2, SA.y $ 198.0 - scaleY * erdos, SA.width 4, SA.height 4]
      , S.g [] $
          pairwise history # mapWithIndex \i (e1 /\ e2) ->
            S.line
              [ SA.x1 $ 10.0 * toNumber (i+1)
              , SA.x2 $ 10.0 * toNumber (i+2)
              , SA.y1 $ 200.0 - scaleY * e1
              , SA.y2 $ 200.0 - scaleY * e2
              , SA.strokeWidth 2.0
              , SA.stroke "blue"
              ]
      ]
    ]


mainTitle :: String
mainTitle = case rules of
  Rules4 -> "Jeu M(5,4) : essayer d'aligner 4 ronds sur cette grille !" 
  Rules5 -> "Jeu M(5,5) : essayer d'aligner 5 ronds sur cette grille !" 

view ∷ Model → Html Msg
view model =
  H.div [H.style "height" "400px", H.style "width" "850px"]
    [
      card mainTitle
        [ H.div [H.class_ "flex flex-row"]
            [ H.div []
                [ viewGrid model
                , H.button [H.class_ buttonClass, E.onClick \_ -> Reinit] [H.text "Recommencer"]
                ]
            , H.div []
                [ viewErdos (model.history <#> _.erdos)
                , case model.status of
                    HasWon -> H.span [] [H.text $ "Bravo, vous avez gagné !"]
                    HasLost -> H.span [] [H.text $ "Dommage, vous avez perdu !"]
                    CannotWin -> H.span [] [H.text "Dommage, X a bloqué tous les alignements, vous avez perdu !"]
                    InProgress -> H.empty
                ]
            ]
        ]
    ]
