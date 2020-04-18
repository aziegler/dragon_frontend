module Main exposing (main)
import Browser
import Model exposing (..)
import View



-- MAIN


main =
  Browser.element { init = Model.init, update = Model.update, subscriptions = subscriptions, view = View.view }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
