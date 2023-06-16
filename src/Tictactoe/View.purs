module Tictactoe.View (view) where

import Relude

import Pha.Html (Html)
import Pha.Html as H
import Pha.Html.Attributes as P
import Pha.Html.Events as E
import Pha.Html.Util (pc, px, translate)
import Pha.Svg as S
import Pha.Svg.Attributes as SA
import Tictactoe.Model (Model, Symb(..))
import Tictactoe.Msg (Msg(..))

buttonClass ∷ String
buttonClass = "py-2.5 px-5 mr-2 mb-2 text-sm font-medium text-gray-900 focus:outline-none bg-white rounded-lg border border-gray-200 hover:bg-gray-100 hover:text-blue-700 focus:z-10 focus:ring-4 focus:ring-gray-200"

checkboxClass ∷ String
checkboxClass = "w-4 h-4 text-blue-600 bg-gray-100 border-gray-300 rounded focus:ring-blue-500 focus:ring-2"

selectClass ∷ String
selectClass = "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5"

inputNumberClass ∷ String
inputNumberClass = "block w-full px-3 py-1.5 text-base font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"

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
        [ H.class_ "absolute cursor-pointer flex items-center justify-center shadow"
        , H.style "height" $ "20%" 
        , H.style "width" $ "20%"
        , H.style "left" $ show (toNumber col * 20.0) <> "%" 
        , H.style "top" $ show (toNumber row * 20.0) <> "%" 
        , H.style "background-color" $ if erdos == -1.0 then "white" else "rgb(255," <> show (255.0 * erdos) <> ",0)"
        , E.onClick \_ -> Play i
        ]
        [ H.span [] [H.text $ case symb of
              X -> "X"
              O -> "O"
              Empty -> ""
          ]
        ]

view ∷ Model → Html Msg
view model =
  H.div [ H.class_ "w-screen flex flex-row justify-around items-start" ]
    [ card "Jeu" [viewGrid model]
    ]
