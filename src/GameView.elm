module GameView exposing (view)
import GameModel exposing (Model, Msg(..))
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import ViewElements exposing (..)


view : Model -> Html Msg
view model =

  layout []<|
    row [height fill, width fill, padding 20, spacing 30]
        [(storyColumn model), (updateColumn model)]


storyColumn : Model -> Element Msg
storyColumn model =
  case model.error of
  Just error -> coloredColumn 6 (rgb255 200 200 200) [(text error),(currentStoryPanel model),(button (Just ResetStory) "Nouvelle histoire")]
  Nothing -> case model.theme of
    Just theme ->
        coloredColumn 6 (rgb255 200 200 200) [(themePanel theme.content),(currentStoryPanel model),(button (Just ResetStory) "Nouvelle histoire")]
    Nothing ->
        coloredColumn 6 (rgb255 200 200 200) ((button (Just ListTheme) "Mettre à jour")::(List.map (\t -> (button (Just (PickTheme t)) t.content)) model.themeList))



updateColumn : Model -> Element Msg
updateColumn model =
  case model.theme of
  Just _ -> coloredColumn 4 (rgb255 170 230 170) [(diceListPanel model),(inputPanel model), (diceRollPanel model)]
  Nothing -> coloredColumn 4 (rgb255 170 230 170) []


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
       [Input.multiline [Element.alignTop, height fill, Font.size 18] {label = (Input.labelHidden "Texte") , placeholder = Nothing, spellcheck = False, onChange = UpdateStory, text = model.currentStory}]

diceRollPanel : Model -> Element Msg
diceRollPanel model =
  row [spacing 5]
      (if model.finished then
        [button (Just ResetStory) ("C'est fini. Appuyer pour recommencer"), button (Just SaveStory) ("Partager cette histoire")]
      else
      [
       button (Just RollNextDice) ("Continuer l'histoire"),
       coloredButton (Just (RollColorDice "Blanc")) (rgb255 255 255 255) ("Lancer le dé blanc"),
       coloredButtonWithFont (Just (RollColorDice "Noir")) (rgb255 0 0 0) (rgb255 255 255 255) ("Lancer le dé noir"),
       column [] []
      ])



mergeDiceAndStory : Model -> List (Element Msg)
mergeDiceAndStory model = List.map renderStory (List.reverse  model.story)
   -- List.reverse (List.map2 (\story -> \dice -> Element.text ("("++dice++") "++story)]) model.story model.dices)
