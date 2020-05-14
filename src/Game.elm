port module Game exposing (main)
import Browser
import GameModel exposing (..)
import GameView
import Json.Decode as Decoder
import Json.Encode as Encoder



-- MAIN


main =
  Browser.element { init = (GameModel.init sendMessage), update = GameModel.update, subscriptions = subscriptions, view = GameView.view }





port sendMessage : Encoder.Value -> Cmd msg
port messageReceiver : (Decoder.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ = messageReceiver NewStory


-- VIEW
