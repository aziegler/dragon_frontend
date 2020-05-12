module GameModel exposing (Model,  update, init, Msg(..))

import Json.Encode as Encoder
import Model exposing (Dice, Theme, displayErr, getDices, getThemes)
import Random
import Array
import Http exposing (jsonBody)
import Json.Decode exposing (..)

type alias Model =
  { dices : List (String,String),
    story : List (String,String),
    currentStory : String,
    currentDice : (String, String),
    theme : Maybe Theme,
    diceList : List Dice,
    themeList : List Theme,
    error : Maybe String,
    finished : Bool
  }



init : () -> (Model, Cmd Msg)
init _ = (initial_model, getThemes ThemeList)


initial_model = Model [] [] "" ("","") Nothing [] [] Nothing False


otherDices = [Dice Nothing "Blanc" ["Et là, nooon", "Et là, Grrrrr", "Et là, Hmmmm", "Et là, Couic", "Et là, Tintintin", "Et là, paf"],
              Dice Nothing "Noir" ["J'adore ce passage","Ah bon? Pourquoi ?","Et ton chien dans tout ça ?","T'as pas eu trop peur ?","Tu veux mon avis ?","Tu peux le prouver ?"]]


type Msg
  = UpdateStory String
  | RollNextDice
  | RollColorDice String
  | ResetStory
  | Saved (Result Http.Error ())
  | PickTheme Theme
  | ListTheme
  | ThemeList (Result Http.Error (List Theme))
  | DiceList (Result Http.Error (List Dice))
  | Rolled (Maybe Dice) Int
  | SaveStory



finish : Model -> String -> (Model, Cmd Msg)
finish model error = ({model | finished = True, error = Just error}, Cmd.none)

rollDice : Dice -> Cmd Msg
rollDice dice =
    let length = List.length dice.faces in
        Random.generate (Rolled (Just dice)) (Random.int 1 (length + 1))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateStory content -> ({ model | currentStory = content }, Cmd.none)
    RollNextDice ->
      if savable model
        then launchDice (saveStory model)
        else (model, Cmd.none)
    RollColorDice color ->
      let diceValues = List.filter (\a -> a.color == color) otherDices in
      case diceValues of
          h::_ -> if savable model
                    then ((saveStory model), rollDice h)
                    else (model, Cmd.none)
          _ -> finish model "Couleur non trouvée"
    Rolled  (Just dice) int ->
      let text = List.head (List.drop (int-1) dice.faces) in
        case text of
           Just content -> (rollNewDice (dice.color, content) model, Cmd.none)
           _ -> finish model "Face non trouvée"
    Rolled Nothing _ -> finish model "Dé non trouvé"
    ResetStory -> init ()
    PickTheme theme -> launchDice {model | theme = Just theme}
    ListTheme -> (model, getThemes ThemeList)
    ThemeList (Ok theme) -> ({model | themeList = theme}   , getDices DiceList)
    ThemeList (Err err) -> displayErr err model
    DiceList (Ok dices) -> ({model | diceList = dices}, Cmd.none)
    DiceList (Err err) -> displayErr err model
    SaveStory -> (model, Http.post {url = "http://localhost:8080/story",body = jsonBody (storyEncoder model.story), expect = Http.expectWhatever Saved})
    Saved (Ok _) -> init()
    Saved (Err err) -> displayErr err model


storyEncoder : List (String,String) -> Encoder.Value
storyEncoder list =
    Encoder.list (\( a, b ) -> Encoder.list identity [ Encoder.string a, Encoder.string b ]) list



launchDice : Model -> (Model, Cmd Msg)
launchDice model =
  case model.diceList of
    h::q -> ({model | diceList = q}, rollDice h)
    _ -> finish model "Plus de dés"


savable : Model -> Bool
savable model =
     String.length (String.trim model.currentStory) > String.length (String.trim (Tuple.second model.currentDice))

saveStory : Model -> Model
saveStory model =
  let formatted_story = String.trim model.currentStory ++". " in
  let (color,_) = model.currentDice in
   { model | story = (color,formatted_story)::model.story, dices = model.currentDice::model.dices, currentDice = ("",""), currentStory = "" }

rollNewDice : (String,String) -> Model -> Model
rollNewDice (color,phrase) model =
  { model | currentDice = (color,phrase), currentStory = phrase}
