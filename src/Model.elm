module Model exposing (Model, Msg, update, init, updateStory, rollNextDice, rollFinalDice, resetStory, pickTheme)

import Random
import Array

type alias Model =
  { dices : List String,
    story : List String,
    currentStory : String,
    currentDice : String,
    theme : Maybe String,
    diceList : List (List String),
    themeList : List String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ok (initial_model)

initial_model = Model [] [] "" "" Nothing diceRollList themes

themes = ["Le cimetière en folie", "Je me suis fait.e virer de l'école des magiciens", "Je me transforme en Loup-Garou tous les lundis"]
diceRollList = [["Quand j'étais petit", "Un beau jour", "Je connais quelqu'un ", "La semaine dernière","Tout le monde pense","Je vous ai jamais dit"],
                ["En plus", "En réalité", "Et croyez-moi", "Et puis", "Alors moi", "Mais vous savez quoi"],
                ["Pas de bol","Quand soudain","Donc, sans hésiter, ","En tout cas","A mon avis", "Alors voilà"],
                ["Sauf que", "Et tenez-vous bien", "Et comme par magie", "Moralité","C'est ainsi que","Comme dirait mon pépé"]]


-- UPDATE
updateStory : String -> Msg
updateStory msg = UpdateStory msg

rollNextDice : Msg
rollNextDice = RollNextDice

rollFinalDice : Msg
rollFinalDice = RollFinalDice

resetStory : Msg
resetStory = ResetStory

pickTheme : String -> Msg
pickTheme = PickTheme

type Msg
  = UpdateStory String
  | RollNextDice
  | ResetStory
  | PickTheme String
  | Rolled Int
  | RollFinalDice

ok : Model -> (Model, Cmd Msg)
ok model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateStory content ->
      ok { model | currentStory = content }

    RollNextDice ->
      if savable model
        then ((saveStory model), Random.generate Rolled (Random.int 1 6))
        else (model, Cmd.none)

    Rolled int ->
      case model.diceList of
      h::q -> let face = List.head (List.drop (int-1) h) in
                  case face of
                      Just text -> ok (rollNewDice text {model | diceList = q})
                      Nothing -> ok (rollNewDice "Forced dice" {model | diceList = q})
      [] -> ok (rollNewDice "Forced dice" model)

    RollFinalDice ->
      ok (rollLastDice (saveStory model))
    ResetStory ->
      ok (initial_model)
    PickTheme theme ->
      ({model | theme = Just theme}, Random.generate Rolled (Random.int 1 6))

savable : Model -> Bool
savable model =
     String.length (String.trim model.currentStory) > String.length (String.trim model.currentDice)

saveStory : Model -> Model
saveStory model =
  let formatted_story = String.trim model.currentStory ++". " in
   { model | story = formatted_story::model.story, dices = model.currentDice::model.dices }

rollNewDice : String -> Model -> Model
rollNewDice phrase model =
  { model | currentDice = phrase, currentStory = phrase}

rollLastDice : Model -> Model
rollLastDice model =
    { model | currentDice = "Pour conclure.. ", currentStory =  "Pour conclure.. "}
