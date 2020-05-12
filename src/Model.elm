module Model exposing (..)

import Http exposing (jsonBody)
import Json.Decode exposing (Decoder, field)
import Json.Encode as Encoder

type alias Theme =
    {
        id: Maybe ObjectId,
        author : String,
        content : String,
        category : String
    }
type alias ObjectId =
    {
        id : String
    }

type alias Dice =
    {
        id : Maybe ObjectId,
        color : String,
        faces : List String
    }

getThemes : ((Result Http.Error (List Theme)) -> msg) -> Cmd msg
getThemes consMsg = Http.get { url = "http://localhost:8080/", expect = Http.expectJson consMsg themeListDecoder }

getDices : ((Result Http.Error (List Dice)) -> msg) -> Cmd msg
getDices consMsg = Http.get { url = "http://localhost:8080/dices", expect = Http.expectJson consMsg diceListDecoder }


displayErr : Http.Error -> {model | error : Maybe String } -> ({model | error : Maybe String }, Cmd msg)
displayErr err model =
        case err of
             Http.BadBody  s -> error model "Bad body"
             Http.BadUrl   s -> error model "Bad URL"
             Http.Timeout -> error model "Time Out"
             Http.NetworkError -> error model "Network Error"
             Http.BadStatus i -> error model "Bad Status"

error : { modelType | error : Maybe String } -> String -> ({ modelType | error : Maybe String }, Cmd msg)
error model errorMsg = ({model | error = Just errorMsg},Cmd.none)


diceRequest : String -> Dice -> ((Result Http.Error (List Dice)) -> msg) -> Cmd msg
diceRequest method dice cons= Http.request {url = "http://localhost:8080/dice", method = method, body = jsonBody (diceEncoder dice), expect = Http.expectJson cons diceListDecoder, headers = [], timeout = Nothing, tracker = Nothing }

themeRequest : String -> Theme -> ((Result Http.Error (List Theme)) -> msg) -> Cmd msg
themeRequest method theme cons = Http.request {url = "http://localhost:8080/theme", method = method, body = jsonBody (themeEncoder theme), expect = Http.expectJson cons themeListDecoder, headers = [], timeout = Nothing, tracker = Nothing }


themeListDecoder : Decoder (List Theme)
themeListDecoder =
    (Json.Decode.list themeDecoder)

diceListDecoder : Decoder (List Dice)
diceListDecoder =
    (Json.Decode.list diceDecoder)

themeDecoder : Decoder Theme
themeDecoder =
    Json.Decode.map4 Theme
        (field "_id" objectIdDecoder)
        (field "author" Json.Decode.string)
        (field "content" Json.Decode.string)
        (field "category" Json.Decode.string)



objectIdDecoder : Decoder (Maybe ObjectId)
objectIdDecoder =
    Json.Decode.map (\s -> Just (ObjectId s)) (field "id" Json.Decode.string)

diceDecoder : Decoder Dice
diceDecoder =
    Json.Decode.map3 Dice
        (field "_id" objectIdDecoder)
        (field "color" Json.Decode.string)
        (field "faces" (Json.Decode.list Json.Decode.string))


themeEncoder : Theme -> Encoder.Value
themeEncoder theme =
    let
        fieldEncoder =
            [ ( "author", Encoder.string theme.author )
            , ( "content", Encoder.string theme.content )
            , ( "category", Encoder.string theme.category )
            ]
    in case objectIdEncoder theme.id of
        Nothing ->  Encoder.object <| fieldEncoder
        Just enc -> Encoder.object <| (("_id",enc)::fieldEncoder)


diceEncoder : Dice -> Encoder.Value
diceEncoder dice =
    let fieldEncoder =
            [ ( "color", Encoder.string dice.color ),
            ( "faces", Encoder.list Encoder.string dice.faces )]

    in case objectIdEncoder dice.id of
            Nothing ->  Encoder.object <| fieldEncoder
            Just enc -> Encoder.object <| (("_id",enc)::fieldEncoder)

objectIdEncoder : Maybe ObjectId -> Maybe Encoder.Value
objectIdEncoder objectId =
    case objectId of
        Nothing -> Nothing
        Just o -> Just (Encoder.object <| [("id", Encoder.string o.id )])

