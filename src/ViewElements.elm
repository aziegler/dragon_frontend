module ViewElements exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


button : Maybe msg ->  String -> Element msg
button msgContent text  = coloredButton msgContent (rgb255 150 150 150) text

coloredButton : Maybe msg -> Color -> String -> Element msg
coloredButton msg color text  = coloredButtonWithFont msg color (rgb255 0 0 0) text

coloredButtonWithFont : Maybe msg -> Color -> Color -> String -> Element msg
coloredButtonWithFont msg color fontColor text  = column [] [Input.button [Background.color color, Border.rounded 5, padding 5, Font.color fontColor] {onPress = msg, label = Element.text text}]



renderDice : (String,String) -> Element msg
renderDice (color, msg) =
  let (bg, fg) = getColors color in
    el[](paragraph[Background.color bg, width (px 90), height (px 90), Border.rounded 5, Font.center, Font.size 16, centerY, padding 5, Font.color fg][(text msg)])



colorList = [("Jaune",(rgb255 255 255 0),(rgb255 0 0 0)),
             ("Orange",(rgb255 217 100 30),(rgb255 0 0 0)),
             ("Rouge",(rgb255 230 20 20),(rgb255 0 0 0)),
             ("Bleu",(rgb255 40 40 217),(rgb255 0 0 0)),
             ("Blanc",(rgb255 255 255 255),(rgb255 0 0 0)),
             ("Violet",(rgb255 130 70 160),(rgb255 0 0 0)),
             ("Noir",(rgb255 0 0 0),(rgb255 255 255 255))]

getColors : String -> (Color,Color)
getColors color = let filtered = List.filter (\(a,b,c) -> a == color) colorList in
    case filtered of
        (a,b,c)::_ -> (b,c)
        _ -> ((rgb255 255 255 255),(rgb255 0 0 0))

renderStory : (String, String) -> Element msg
renderStory (color, story ) =
  let (bg, fg) = getColors color in
      paragraph[Background.color bg, Font.color fg][text (story)]


coloredColumn : Int -> Color -> List (Element msg) -> Element msg
coloredColumn widthPortion color l = column [height fill, width <| fillPortion widthPortion, Border.rounded 20, Background.color color, padding 30, spacing 10 ] l
