module Model exposing (Model, Msg, update, init, updateStory, rollNextDice, rollFinalDice, resetStory)

import Random
import Array

type alias Model =
  { dices : List String,
    story : List String,
    currentStory : String,
    currentDice : String,
    theme : String,
    diceList : List (List String)
  }

init : () -> (Model, Cmd Msg)
init _ =
  ok (initial_model)

initial_model = Model [] [] "Ce jour-là" "Ce jour-là" "Salade de rêves" diceRollList

themes = ["Le cimetière en folie", "Je me suis fait.e virer de l'école des magiciens"]
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

type Msg
  = UpdateStory String
  | RollNextDice
  | ResetStory
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
      (model, Random.generate Rolled (Random.int 1 6))

    Rolled int ->
      case model.diceList of
      h::q -> let face = List.head (List.drop (int-1) h) in
                  case face of
                      Just text -> ok (rollNewDice text (saveStory {model | diceList = q}))
                      Nothing -> ok (rollNewDice "Forced dice" (saveStory {model | diceList = q}))
      [] -> ok (rollNewDice "Forced dice" (saveStory model))

    RollFinalDice ->
      ok (rollLastDice (saveStory model))
    ResetStory ->
      ok (initial_model)

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
