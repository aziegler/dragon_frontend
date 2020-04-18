module Main exposing (main)
import Browser
import Model
import View



-- MAIN


main =
  Browser.element { init = Model.init, update = Model.update, subs = subscriptions, view = View.view }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
