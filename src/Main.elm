import Html exposing (div, table, td, text, Html, tr, h1, img, label, input, button, p)
import Html.App as Html
import Html.Attributes exposing (src, type', checked)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as Json exposing ( (:=) )
import Task

type alias Channel =
  { status : String
  , displayName : String
  }
type alias StreamPreview =
  { small : String
  , medium : String
  , large : String
  }
type alias Stream =
  { channel : Channel
  , preview : StreamPreview
  }

type alias Model =
  { loading : Bool
  , game : String
  , streams : List Stream
  , previewChoice : PreviewOption
  }

type PreviewOption
  = None
  | Small
  | Medium
  | Large

init : String -> (Model, Cmd Action)
init game =
  (Model False game [] None, getStreams "dota 2")

main =
  Html.program
    { init = init "dota 2"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Action
  = Reload
  | FetchSucceed (List Stream)
  | FetchFail Http.Error
  | ChangePreview PreviewOption

subscriptions : Model -> Sub Action
subscriptions model =
  Sub.none

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Reload ->
      ( { model | loading = True }
      , getStreams model.game )
    FetchSucceed streams ->
      ( { model | streams = streams, loading = False }
      , Cmd.none )
    FetchFail err ->
      ( { model | loading = False }
      , Cmd.none )
    ChangePreview choice ->
      ( { model | previewChoice = choice }
      , Cmd.none )

previewHtml : Model -> Stream -> Html Action
previewHtml model stream =
  case model.previewChoice of
    None ->
      div [] []
    Small ->
      img [src stream.preview.small] []
    Medium ->
      img [src stream.preview.medium] []
    Large ->
      img [src stream.preview.large] []

radio : PreviewOption -> String -> Model -> Html Action
radio choice name model =
  let
    isSelected = model.previewChoice == choice
  in
    label []
      [ input [ type' "radio", checked isSelected, onCheck (\_ -> ChangePreview choice) ] []
      , text name
      ]

loadingState : Model -> String
loadingState model =
  if model.loading then
    "Loading..."
  else
    "Done"

view : Model -> Html Action
view model =
  let
    fancyMakeRow : Stream -> Html Action
    fancyMakeRow stream =
      tr [] [ td [] [ previewHtml model stream ]
            , td [] [text stream.channel.displayName]
            , td [] [text stream.channel.status]
            ]
    rows = List.map fancyMakeRow <| model.streams
  in
    div []
      [ h1 [] [ text model.game
              , button [ onClick Reload ] [ text "Reload" ]
              ]
      , p [] [ text ("Loading State: " ++ (loadingState model)) ]
      , radio None "None" model
      , radio Small "Small" model
      , radio Medium "Medium" model
      , radio Large "Large" model
      , table [] rows
      ]

getStreams : String -> Cmd Action
getStreams game =
  let
    url = Http.url "https://api.twitch.tv/kraken/streams" [ ("game", game)
                                                          , ("language", "en") ]
  in
    Task.perform FetchFail FetchSucceed (Http.get streamsDecoder url)

channelDecoder : Json.Decoder Channel
channelDecoder =
  Json.object2 Channel
    ("status" := Json.string)
    ("display_name" := Json.string)

streamDecoder : Json.Decoder Stream
streamDecoder =
  Json.object2 Stream
    ("channel" := channelDecoder)
    ("preview" := previewDecoder)

previewDecoder : Json.Decoder StreamPreview
previewDecoder =
  Json.object3 StreamPreview
    ("small" := Json.string)
    ("medium" := Json.string)
    ("large" := Json.string)

streamsDecoder : Json.Decoder (List Stream)
streamsDecoder =
  ("streams" := Json.list streamDecoder)
