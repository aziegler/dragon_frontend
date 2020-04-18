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
    row [height fill, width fill, padding 20, spacing 30] [(storyColumn model), (updateColumn model)]

storyColumn : Model -> Element Msg
storyColumn model =
  column [height fill, width <| fillPortion 6, Border.rounded 20, Background.color (rgb255 200 200 200), padding 30 ]
  [(themePanel model),(currentStoryPanel model),(button (Just Model.resetStory) "Nouvelle histoire")]

updateColumn : Model -> Element Msg
updateColumn model =
  column [height fill, width <| fillPortion 4, Border.rounded 20, Background.color (rgb255 170 230 170), padding 30]
  [(diceListPanel model),(inputPanel model), (diceRollPanel model)]


-- Story

themePanel : Model -> Element Msg
themePanel model =
  row [height (px 40), width fill,  Background.color (rgb255 150 150 150), Border.rounded 5, padding 5]
      [el[centerX, centerY] (text ("Thème : "++model.theme))]

currentStoryPanel : Model -> Element Msg
currentStoryPanel model =
  row [height fill, width fill]
      <| [el[alignTop, width fill, Font.justify] (Element.paragraph [] (mergeDiceAndStory model))]


-- Input
diceListPanel : Model -> Element Msg
diceListPanel model =
   let diceList = List.map (\d -> renderDice d (rgb255 230 170 170)) (List.reverse(model.currentDice::model.dices)) in
   row[height (px 100), width fill,  spacing 10 ] diceList

inputPanel : Model -> Element Msg
inputPanel model =
  row [height (px 200), width fill, padding 10]
       [Input.multiline [Element.alignTop, height fill, Font.size 18] {label = (Input.labelHidden "Texte") , placeholder = Nothing, spellcheck = False, onChange = Model.updateStory, text = model.currentStory}]


button : Maybe Msg ->  String -> Element Msg
button msg text  = column [] [Input.button [Background.color (rgb255 150 150 150), Border.rounded 5, padding 5] {onPress = msg, label = Element.text text}]

diceRollPanel : Model -> Element Msg
diceRollPanel model =
  row [][
       button (Just Model.rollNextDice) ("Continuer l'histoire"),
       column [] []]

renderDice : String -> Color -> Element Msg
renderDice msg color = el[](paragraph[Background.color color, width (px 70), height (px 70), Border.rounded 5, Font.center, Font.size 16, centerY, padding 5][(text msg)])




mergeDiceAndStory : Model -> List (Element Msg)
mergeDiceAndStory model = List.reverse (List.map2 (\story -> \dice -> paragraph[Border.rounded 5][text (story)]) model.story model.dices)
   -- List.reverse (List.map2 (\story -> \dice -> Element.text ("("++dice++") "++story)]) model.story model.dices)
