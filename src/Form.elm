module Form exposing (Model, PreviewOption(..), Msg, update, view, init)

import Html exposing (form, input, div, Html, label, text)
import Html.Attributes exposing (value, type', checked)
import Html.Events exposing (onInput, onCheck, onSubmit)

type alias Model =
  { game : String
  , previewOption : PreviewOption
  }

type PreviewOption
  = None
  | Small
  | Medium
  | Large

init : Model
init =
  Model
    "dota 2"
    None

type Msg
  = UpdateGame String
  | ChangePreview PreviewOption
  | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateGame newGame -> { model | game = newGame }
    ChangePreview newPreview -> { model | previewOption = newPreview }
    Submit -> model

view : Model -> Html Msg
view model =
  form
    [ onSubmit Submit
    ]
    [ input
      [ value model.game
      , onInput UpdateGame
      ] []
    , div []
      [ radio None "None" model
      , radio Small "Small" model
      , radio Medium "Medium" model
      , radio Large "Large" model
      ]
    ]

radio : PreviewOption -> String -> Model -> Html Msg
radio choice name model =
  let
    isSelected = model.previewOption == choice
  in
    label []
      [ input
        [ type' "radio"
        , checked isSelected
        , onCheck (\_ -> ChangePreview choice)
        ] []
      , text name
      ]
