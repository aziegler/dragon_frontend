module View exposing (view)
import Model exposing (Model, Msg)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


view : Model -> Html Msg
view model =
  layout []<|
    row [height fill, width fill, padding 20, spacing 30]
        [(storyColumn model), (updateColumn model)]

storyColumn : Model -> Element Msg
storyColumn model =
  case model.theme of
    Just themeText ->
      column [height fill, width <| fillPortion 6, Border.rounded 20, Background.color (rgb255 200 200 200), padding 30 ]
      [(themePanel themeText),(currentStoryPanel model),(button (Just Model.resetStory) "Nouvelle histoire")]
    Nothing -> column [height fill, width <| fillPortion 6, Border.rounded 20, Background.color (rgb255 200 200 200), padding 30, spacing 10 ]
       (List.map (\t -> (button (Just (Model.pickTheme t)) t)) model.themeList)


updateColumn : Model -> Element Msg
updateColumn model =
  case model.theme of
  Just _ -> column [height fill, width <| fillPortion 4, Border.rounded 20, Background.color (rgb255 170 230 170), padding 30]
            [(diceListPanel model),(inputPanel model), (diceRollPanel model)]
  Nothing -> column [height fill, width <| fillPortion 4, Border.rounded 20, Background.color (rgb255 170 230 170), padding 30] []


-- Story

themePanel : String -> Element Msg
themePanel theme =
  row [height (px 40), width fill,  Background.color (rgb255 150 150 150), Border.rounded 5, padding 5]
      [el[centerX, centerY] (text ("Thème : "++theme))]

currentStoryPanel : Model -> Element Msg
currentStoryPanel model =
  row [height fill, width fill]
      <| [el[alignTop, width fill, Font.justify] (Element.paragraph [] (mergeDiceAndStory model))]


-- Input
diceListPanel : Model -> Element Msg
diceListPanel model =
    let diceList = List.map renderDice (List.reverse(model.currentDice::model.dices))  in
      row[height (px 100), width fill,  spacing 10 ] diceList

inputPanel : Model -> Element Msg
inputPanel model =
  row [height (px 200), width fill, padding 10]
       [Input.multiline [Element.alignTop, height fill, Font.size 18] {label = (Input.labelHidden "Texte") , placeholder = Nothing, spellcheck = False, onChange = Model.updateStory, text = model.currentStory}]


button : Maybe Msg ->  String -> Element Msg
button msg text  = coloredButton msg (rgb255 150 150 150) text

coloredButton : Maybe Msg -> Color -> String -> Element Msg
coloredButton msg color text  = coloredButtonWithFont msg color (rgb255 0 0 0) text

coloredButtonWithFont : Maybe Msg -> Color -> Color -> String -> Element Msg
coloredButtonWithFont msg color fontColor text  = column [] [Input.button [Background.color color, Border.rounded 5, padding 5, Font.color fontColor] {onPress = msg, label = Element.text text}]

diceRollPanel : Model -> Element Msg
diceRollPanel model =
  row [spacing 5][
       button (Just Model.rollNextDice) ("Continuer l'histoire"),
       coloredButton (Just (Model.rollColorDice "Blanc")) (rgb255 255 255 255) ("Lancer le dé blanc"),
       coloredButtonWithFont (Just (Model.rollColorDice "Noir")) (rgb255 0 0 0) (rgb255 255 255 255) ("Lancer le dé noir"),
       column [] []]

renderDice : (String,String) -> Element Msg
renderDice (color, msg) =
  let rgbColor = List.filter (\(a,b,c) -> a == color) colorList in
  case rgbColor of
    (a,b,c)::_ -> el[](paragraph[Background.color b, width (px 90), height (px 90), Border.rounded 5, Font.center, Font.size 16, centerY, padding 5, Font.color c][(text msg)])
    [] -> el[](paragraph[Background.color (rgb255 10 10 10), width (px 90), height (px 90), Border.rounded 5, Font.center, Font.size 16, centerY, padding 5][(text msg)])


colorList = [("Jaune",(rgb255 255 255 0),(rgb255 0 0 0)),
             ("Orange",(rgb255 217 100 30),(rgb255 0 0 0)),
             ("Rouge",(rgb255 230 170 170),(rgb255 0 0 0)),
             ("Bleu",(rgb255 40 40 217),(rgb255 0 0 0)),
             ("Blanc",(rgb255 255 255 255),(rgb255 0 0 0)),
             ("Noir",(rgb255 0 0 0),(rgb255 255 255 255))]

renderStory : (String, String) -> Element Msg
renderStory (color, story ) =
  let rgbColor = List.filter (\(a,b,c) -> a == color) colorList in
      case rgbColor of
        (a,b,c)::_ -> paragraph[Background.color b, Font.color c][text (story)]
        _ -> paragraph[][text (story)]

mergeDiceAndStory : Model -> List (Element Msg)
mergeDiceAndStory model = List.map renderStory (List.reverse  model.story)
   -- List.reverse (List.map2 (\story -> \dice -> Element.text ("("++dice++") "++story)]) model.story model.dices)
