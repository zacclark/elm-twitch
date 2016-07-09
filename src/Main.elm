import Html exposing (div, table, td, text, Html, tr, h1, img, label, input, button, p)
import Html.App as Html
import Html.Attributes exposing (src, type', checked)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as Json exposing ( (:=) )
import Task

import Form

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
  , streams : List Stream
  , form : Form.Model
  }

init : (Model, Cmd Action)
init =
  let
    form = Form.init
  in
    (Model False [] form, getStreams form.game)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Action
  = Reload
  | FetchSucceed (List Stream)
  | FetchFail Http.Error
  | ChangeForm Form.Msg

subscriptions : Model -> Sub Action
subscriptions model = Sub.none

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    Reload ->
      ( { model | loading = True }
      , getStreams model.form.game )
    FetchSucceed streams ->
      ( { model | streams = streams, loading = False }
      , Cmd.none )
    FetchFail err ->
      ( { model | loading = False }
      , Cmd.none )
    ChangeForm formMsg ->
      ( { model | form = (Form.update formMsg model.form) }
      , Cmd.none )

previewUrl : Model -> Stream -> Maybe String
previewUrl model stream =
  case model.form.previewOption of
    Form.None -> Nothing
    Form.Small -> Just stream.preview.small
    Form.Medium -> Just stream.preview.medium
    Form.Large -> Just stream.preview.large

previewHtml : Model -> Stream -> Html Action
previewHtml model stream =
  case previewUrl model stream of
    Nothing -> div [] []
    Just url -> img [src url] []

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
            , td [] [ text stream.channel.displayName ]
            , td [] [ text stream.channel.status ]
            ]
    rows = List.map fancyMakeRow <| model.streams
  in
    div []
      [ h1 [] [ text model.form.game
              , button [ onClick Reload ] [ text "Reload" ]
              ]
      , p [] [ text ("Loading State: " ++ (loadingState model)) ]
      , (Html.map ChangeForm (Form.view model.form))
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
