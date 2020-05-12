module Editor exposing (main)
import Browser
import EditorModel exposing (EditorModel, EditorMsg, initEditor, updateEditor)
import EditorView



-- MAIN


main =
  Browser.element { init = initEditor, update = updateEditor, subscriptions = subscriptions, view = EditorView.view }


subscriptions : EditorModel -> Sub EditorMsg
subscriptions model =
  Sub.none


-- VIEW
