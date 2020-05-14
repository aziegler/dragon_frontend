module GameModel exposing (Model,  update, init, Msg(..), categoryList)

import Json.Decode as Decoder exposing (Decoder, decodeValue, errorToString, field)
import Json.Encode as Encoder
import Model exposing (Dice, ObjectId, Theme, displayErr, error, getDices, getThemes, objectIdDecoder, objectIdEncoder)
import Random
import Http exposing (jsonBody)

type alias Model =
  { dices : List (String,String),
    story : Story,
    currentStory : String,
    currentDice : (String, String),
    theme : Maybe Theme,
    category : Maybe String,
    diceList : List Dice,
    themeList : List Theme,
    error : Maybe String,
    message_sender : Encoder.Value -> Cmd Msg
  }

type alias Story =
  {
        id : Maybe ObjectId,
        theme : String,
        story : List (String, String),
        finished : Bool
  }



init : (Encoder.Value -> Cmd Msg) -> () -> (Model, Cmd Msg)
init sendMsg _ = (initial_model sendMsg, getThemes ThemeList)


initial_model sendMsg = Model [] (Story Nothing "" [] False) "" ("","") Nothing Nothing [] [] Nothing sendMsg


otherDices = [Dice Nothing "Blanc" ["Et là, nooon", "Et là, Grrrrr", "Et là, Hmmmm", "Et là, Couic", "Et là, Tintintin", "Et là, paf"],
              Dice Nothing "Noir" ["J'adore ce passage","Ah bon? Pourquoi ?","Et ton chien dans tout ça ?","T'as pas eu trop peur ?","Tu veux mon avis ?","Tu peux le prouver ?"]]


type Msg
  = UpdateStory String
  | RollNextDice
  | RollColorDice String
  | ResetStory
  | Saved (Result Http.Error ())
  | PickTheme Theme
  | PickCategory String
  | ListTheme
  | ThemeList (Result Http.Error (List Theme))
  | DiceList (Result Http.Error (List Dice))
  | Rolled (Maybe Dice) Int
  | NewStory Decoder.Value
  | SaveStory



finish : Model -> String -> (Model, Cmd Msg)
finish model error =
    let currentStory = model.story in
    let newModel = {model | story = {currentStory | finished = True}, error = Just error} in (newModel, sendStory newModel)

rollDice : Dice -> Cmd Msg
rollDice dice =
    let length = List.length dice.faces in
        Random.generate (Rolled (Just dice)) (Random.int 1 length)

sendStory : Model -> Cmd Msg
sendStory model = model.message_sender (storyEncoder model.story)

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
                    then ((saveStory model), Cmd.batch[rollDice h,sendStory model])
                    else (model, Cmd.none)
          _ -> finish model "Couleur non trouvée"
    Rolled  (Just dice) int ->
      let text = List.head (List.drop (int-1) dice.faces) in
        case text of
           Just content -> ((rollNewDice (dice.color, content) model), Cmd.none)
           _ -> finish model ("Rolled a "++(String.fromInt int)++" on a "++String.fromInt (List.length dice.faces)++" faced dice")
    Rolled Nothing _ -> finish model "Dé non trouvé"
    ResetStory -> init model.message_sender ()
    PickTheme theme -> launchDice (setTheme model theme)
    PickCategory categoryName -> ({model | category = Just categoryName}, Cmd.none)
    ListTheme -> (model, getThemes ThemeList)
    ThemeList (Ok theme) -> ({model | themeList = theme}   , getDices DiceList)
    ThemeList (Err err) -> displayErr err model
    DiceList (Ok dices) -> ({model | diceList = dices}, Cmd.none)
    DiceList (Err err) -> displayErr err model
    SaveStory -> (model, Http.post {url = "http://localhost:8080/story",body = jsonBody (storyEncoder model.story), expect = Http.expectWhatever Saved})
    Saved (Ok _) -> init model.message_sender ()
    NewStory content ->
        case (decodeValue storyDecoder content) of
            Ok story -> let theme = List.filter (\t -> t.content == story.theme) model.themeList in
              case theme of
                  h::_ -> ({model | story = story, theme = Just h }, Cmd.none)
                  [] -> ({model | story = story, theme = Just (Theme Nothing "" story.theme "") }, Cmd.none)
            Err err ->  error model (errorToString err)
    Saved (Err err) -> displayErr err model


setTheme : Model -> Theme -> Model
setTheme model theme = let story = model.story in
    let newStory = {story | theme = theme.content } in
      {model | theme = Just theme, story = newStory}

storyEncoder : Story -> Encoder.Value
storyEncoder story =
  let fieldEncoder = [ ( "theme", Encoder.string story.theme ),
                       ( "story", Encoder.list (\( a, b ) -> Encoder.list identity [ Encoder.string a, Encoder.string b ]) story.story ),
                       ( "finished", Encoder.bool story.finished ) ]
    in case objectIdEncoder story.id of
        Nothing ->  Encoder.object <| fieldEncoder
        Just enc -> Encoder.object <| (("_id",enc)::fieldEncoder)





storyDecoder : Decoder Story
storyDecoder =
    Decoder.map4 Story
        (field "_id" objectIdDecoder)
        (field "theme" Decoder.string)
        (field "story" (Decoder.list (Decoder.map2 Tuple.pair (Decoder.index 0 Decoder.string) (Decoder.index 1 Decoder.string))))
        (field "finished" Decoder.bool)



categoryList : Model -> List String
categoryList model = let categories = List.map (\t -> t.category) model.themeList in List.foldl (\s -> \l -> if (List.member s l) then l else s::l ) [] categories

launchDice : Model -> (Model, Cmd Msg)
launchDice model =
  case model.diceList of
    h::q -> ({model | diceList = q}, Cmd.batch [rollDice h, sendStory model])
    _ -> finish model "Plus de dés"


savable : Model -> Bool
savable model =
     String.length (String.trim model.currentStory) > String.length (String.trim (Tuple.second model.currentDice))

saveStory : Model -> Model
saveStory model =
  let formatted_story = String.trim model.currentStory ++". " in
  let (color,_) = model.currentDice in
  let oldStory = model.story in
  let newStory = { oldStory | story = (color,formatted_story)::(oldStory.story) } in
   { model | story = newStory , dices = model.currentDice::model.dices, currentDice = ("",""), currentStory = "" }

rollNewDice : (String,String) -> Model -> Model
rollNewDice (color,phrase) model =
  { model | currentDice = (color,phrase), currentStory = phrase}
