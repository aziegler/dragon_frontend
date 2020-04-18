module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { dices : List String,
    story : List String,
    currentStory : String,
    currentDice : String,
    theme : String
  }


init : Model
init =
  Model ["Il était une fois"] [] "Ce jour-là" "Ce jour-là" "C'est un thème quoi"



-- UPDATE


type Msg
  = UpdateStory String
  | RollNextDice
  | RollFinalDice



update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateStory content ->
      { model | currentStory = content }

    RollNextDice ->
      rollNewDice (saveStory model)

    RollFinalDice ->
      rollFinalDice (saveStory model)

saveStory : Model -> Model
saveStory model =
   { model | story = model.currentStory::model.story, dices = model.currentDice::model.dices }

rollNewDice : Model -> Model
rollNewDice model =
  { model | currentDice = "Et à ce moment-là ...", currentStory = "Et à ce moment-là ..."}

rollFinalDice : Model -> Model
rollFinalDice model =
    { model | currentDice = "Pour conclure.. ", currentStory =  "Pour conclure.. "}

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [][text "Thème"]
     ,div [][text model.theme]
     ,div [][text "Jusque là"]
     ,div [] (mergeDiceAndStory model)
    , input [placeholder model.currentDice, value model.currentStory, onInput UpdateStory][]
    , div[][button [onClick RollNextDice][text "Nouveau dé"],button [onClick RollFinalDice][text "Conclure"]]
    ]

mergeDiceAndStory : Model -> List (Html Msg)
mergeDiceAndStory model =
    List.reverse (List.map2 (\story -> \dice -> span [] [text ("("++dice++") "++story)]) model.story model.dices)
