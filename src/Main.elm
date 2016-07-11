import Html exposing (div, table, td, text, Html, tr, h1, img, label, input, button, p)
import Html.App as Html
import Html.Attributes exposing (src, type', checked)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as Json exposing ( (:=) )
import Task

import Streams

type alias Model =
  { streams : Streams.Model
  }

init : (Model, Cmd Msg)
init =
  let
    (streams, streamCmd) = Streams.init "dota 2"
  in
    ( Model streams
    , Cmd.batch [ Cmd.map StreamsChange streamCmd ] )

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg
  = StreamsChange Streams.Msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    StreamsChange streamsMsg ->
      let
        (newStreams, newCmd) = Streams.update streamsMsg model.streams
      in
        ( { model | streams = newStreams }
        , Cmd.map StreamsChange newCmd )

view : Model -> Html Msg
view model =
  div []
    [ Html.map StreamsChange (Streams.view model.streams)
    ]
