module Game exposing (main)
import Browser
import GameModel exposing (..)
import GameView



-- MAIN


main =
  Browser.element { init = GameModel.init, update = GameModel.update, subscriptions = subscriptions, view = GameView.view }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
