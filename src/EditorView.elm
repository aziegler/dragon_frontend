module EditorView exposing (view)

import EditorModel exposing (EditorModel, EditorMsg(..))
import Element exposing (..)

import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Model exposing (Dice, Theme)
import ViewElements exposing (button, coloredColumn, getColors)



view : EditorModel -> Html EditorMsg
view model =
     let errorMsg =
          case model.error of
            Just error -> [text error]
            Nothing -> []
     in
     layout []<|
        column [height fill, width fill]
        [row [height fill, width fill, padding 20, spacing 30]
        [manageThemes model,  manageDices model],
        row [] errorMsg ]

manageThemes : EditorModel -> Element EditorMsg
manageThemes model =
    coloredColumn 5 (rgb255 200 200 200) ((newTheme model.newTheme)::(List.map displayTheme model.themeList))

manageDices : EditorModel -> Element EditorMsg
manageDices model =
    coloredColumn 5 (rgb255 206 160 25) ((newDice model.newDice)::(List.map displayDice model.diceList))

newTheme : Theme -> Element EditorMsg
newTheme theme =    column [spacing 10]
                  [row [height fill, width fill, spacing 12]
                  [ themeInput (\a -> UpdateTheme {theme | author = a}) "Author" theme.author,
                    themeInput (\c -> UpdateTheme {theme | category = c}) "Category" theme.category,
                    themeInput (\c -> UpdateTheme {theme | content = c}) "Content" theme.content
                  ],
                  row [height fill, width fill, padding 5, spacing 10]
                    [button (Just SaveTheme) "Sauver"]]

newDice : Dice -> Element EditorMsg
newDice dice = column [spacing 10]
                  [row [height fill, width fill, spacing 12]
                  [ themeInput (\a -> UpdateDice {dice | color = a}) "Couleur" dice.color,
                    themeInput (\c -> UpdateDice {dice | faces = (String.split "," c)}) "Face" (String.join ","(dice.faces))
                  ],
                  row [height fill, width fill, padding 5, spacing 10]
                    [button (Just SaveDice) "Sauver"]]

themeInput : (String -> EditorMsg) -> String -> String -> Element EditorMsg
themeInput  msg label content = Input.text [padding 2] {label=(Input.labelLeft [centerY] (text label)),onChange = msg, text = content, placeholder = Nothing}

displayDice : Dice -> Element EditorMsg
displayDice dice =
    let (bg,fg) = getColors (dice.color) in
    let diceFaces = List.map (\d -> text (d++", ")) (dice.faces) in
                column [height shrink, width fill, padding 5, Background.color bg, Font.color fg, spacing 10, Border.rounded 5]
                [row [ spacing 10]
                       (List.concat [[text "Color :", text dice.color, text "Faces :"],diceFaces]),
                 row [ spacing 10] [button (Just (EditDice dice)) "Editer", button (Just (DeleteDice dice)) "Supprimer"]]

displayTheme : Theme -> Element EditorMsg
displayTheme theme = row [height shrink, width fill, padding 5, spacing 10]
                         [text "Author :", text theme.author, text "Category :", text theme.category, text "Content:", text theme.content, button (Just (EditTheme theme)) "Editer", button (Just (DeleteTheme theme)) "Supprimer"]