module Model exposing (Model, Msg, update, init, updateStory, rollNextDice, rollFinalDice)

import Random

type alias Model =
  { dices : List String,
    story : List String,
    currentStory : String,
    currentDice : String,
    theme : String
  }


init : Model
init =
  ok (Model [] [] "Ce jour-là" "Ce jour-là" "Salade de rêves")


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

type Msg
  = UpdateStory String
  | RollNextDice Int
  | RollFinalDice

ok : Model -> (Model, Cmd Msg)
ok model = (Success model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateStory content ->
      ok { model | currentStory = content }

    RollNextDice ->
      ok rollNewDice (saveStory model)

    RollFinalDice ->
      ok rollLastDice (saveStory model)

saveStory : Model -> Model
saveStory model =
   { model | story = model.currentStory::model.story, dices = model.currentDice::model.dices }

rollNewDice : Model -> Model
rollNewDice model =
  { model | currentDice = "Et à ce moment-là ...", currentStory = "Et à ce moment-là ..."}

rollLastDice : Model -> Model
rollLastDice model =
    { model | currentDice = "Pour conclure.. ", currentStory =  "Pour conclure.. "}
