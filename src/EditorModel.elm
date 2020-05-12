module EditorModel exposing (..)

import Http
import Model exposing (Dice, Theme, diceListDecoder, diceRequest, displayErr, getDices, getThemes, themeListDecoder, themeRequest)


type alias EditorModel =
    {themeList : List Theme,
     diceList : List Dice,
     newTheme : Theme,
     newDice : Dice,
     error : Maybe String}

initEditor : () -> (EditorModel, Cmd EditorMsg)
initEditor _ = ({themeList = [], diceList = [], newTheme = Theme Nothing "" "" "", newDice = Dice Nothing "" [], error = Nothing}, getThemes ThemeList )



updateEditor : EditorMsg -> EditorModel -> (EditorModel, Cmd EditorMsg)
updateEditor msg model =
    case msg of
        ThemeList (Ok list) -> ({model | themeList = list}, getDices DiceList)
        DiceList (Ok list) -> ({model | diceList = list}, Cmd.none)
        ThemeList (Err err) -> displayErr err model
        DiceList (Err err) -> displayErr err model
        UpdateTheme theme -> ({model| newTheme = theme}, Cmd.none)
        EditTheme theme -> ({model | newTheme = theme}, Cmd.none)
        SaveTheme -> ({model | newTheme =  Theme Nothing "" "" ""} , themeRequest "POST" model.newTheme ThemeList)
        SaveDice -> ({model | newDice = Dice Nothing "" []} , diceRequest "POST" model.newDice DiceList)
        DeleteTheme theme -> (model, themeRequest "DELETE" theme ThemeList)
        UpdateDice dice -> ({model | newDice = dice}, Cmd.none)
        EditDice dice -> ({model | newDice = dice}, Cmd.none)
        DeleteDice dice -> (model, diceRequest "DELETE" dice DiceList)


type EditorMsg =
        ThemeList (Result Http.Error (List Theme))
        |DiceList (Result Http.Error (List Dice))
        |UpdateTheme Theme
        |UpdateDice Dice
        |EditTheme Theme
        |DeleteTheme Theme
        |EditDice Dice
        |DeleteDice Dice
        |SaveTheme
        |SaveDice







