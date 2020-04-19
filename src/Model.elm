module Model exposing (Model, Msg, update, init, updateStory, rollNextDice, resetStory, pickTheme, rollColorDice)

import Random
import Array

type alias Model =
  { dices : List (String,String),
    story : List (String,String),
    currentStory : String,
    currentDice : (String, String),
    theme : Maybe String,
    diceList : List (String, List String),
    themeList : List String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ok (initial_model)

initial_model = Model [] [] "" ("","") Nothing diceRollList themes

themes = ["Le cimetière en folie", "Je me suis fait.e virer de l'école des magiciens", "Je me transforme en Loup-Garou tous les lundis", "Gaël apprend à programmer"]
diceRollList = [
                  ("Jaune", ["Quand j'étais petit", "Un beau jour", "Je connais quelqu'un ", "La semaine dernière","Tout le monde pense","Je vous ai jamais dit"]),
                  ("Orange", ["En plus", "En réalité", "Et croyez-moi", "Et puis", "Alors moi", "Mais vous savez quoi"]),
                  ("Rouge", ["Pas de bol","Quand soudain","Donc, sans hésiter, ","En tout cas","A mon avis", "Alors voilà"]),
                  ("Bleu", ["Sauf que", "Et tenez-vous bien", "Et comme par magie", "Moralité","C'est ainsi que","Comme dirait mon pépé"])
              ]

otherDices = [("Blanc",["Et là, nooon", "Et là, Grrrrr", "Et là, Hmmmm", "Et là, Couic", "Et là, Tintintin", "Et là, paf"]),
             ("Noir",["J'adore ce passage","Ah bon? Pourquoi ?","Et ton chien dans tout ça ?","T'as pas eu trop peur ?","Tu veux mon avis ?","Tu peux le prouver ?"])]


-- UPDATE
updateStory : String -> Msg
updateStory msg = UpdateStory msg

rollNextDice : Msg
rollNextDice = RollNextDice

rollColorDice :String -> Msg
rollColorDice = RollColorDice

resetStory : Msg
resetStory = ResetStory

pickTheme : String -> Msg
pickTheme = PickTheme





type Msg
  = UpdateStory String
  | RollNextDice
  | RollColorDice String
  | ResetStory
  | PickTheme String
  | Rolled (Maybe (String, List String)) Int

ok : Model -> (Model, Cmd Msg)
ok model = (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateStory content ->
      ok { model | currentStory = content }

    RollNextDice ->
      if savable model
        then launchDice (saveStory model)
        else (model, Cmd.none)

    RollColorDice color ->
      let diceValues = List.filter (\a -> Tuple.first a == color) otherDices in
      case diceValues of
          h::_ -> if savable model
                    then ((saveStory model), Random.generate (Rolled (Just h)) (Random.int 1 6))
                    else (model, Cmd.none)
          _ -> (model, Cmd.none)

    Rolled (Just (color, faces)) int ->
      let text = List.head (List.drop (int-1) faces) in
        case text of
           Just content -> ok (rollNewDice (color, content) model)
           _ -> ok (rollNewDice ("None","Forced dice") model)
    Rolled Nothing int -> ok (rollNewDice ("None","Forced dice") model)

    ResetStory ->
      ok (initial_model)
    PickTheme theme ->
      launchDice {model | theme = Just theme}

launchDice : Model -> (Model, Cmd Msg)
launchDice model =
  case model.diceList of
    h::q -> ({model | diceList = q}, Random.generate (Rolled (List.head model.diceList)) (Random.int 1 6))
    _ -> (model, Cmd.none)


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
