import Html exposing (Html, div, hr)
import Html.App as Html
import Html.Attributes exposing (src, type', checked)
import Html.Events exposing (onCheck, onClick)
import Http
import Json.Decode as Json exposing ( (:=) )
import Task

import Games
import Streams

type alias Model =
  { games : Games.Model
  , streams : Streams.Model
  }

init : (Model, Cmd Msg)
init =
  let
    game = "dota 2"
    (games, gamesCmd) = Games.init game
    (streams, streamCmd) = Streams.init game
  in
    ( Model games streams
    , Cmd.batch
        [ Cmd.map GamesChange gamesCmd
        , Cmd.map StreamsChange streamCmd ]
    )

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg
  = GamesChange Games.Msg
  | StreamsChange Streams.Msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    GamesChange gamesMsg ->
      let
        (newGames, newCmd) = Games.update gamesMsg model.games
      in
        ( { model | games = newGames }
        , Cmd.map GamesChange newCmd )

    StreamsChange streamsMsg ->
      let
        (newStreams, newCmd) = Streams.update streamsMsg model.streams
      in
        ( { model | streams = newStreams }
        , Cmd.map StreamsChange newCmd )

view : Model -> Html Msg
view model =
  div []
    [ Html.map GamesChange (Games.view model.games)
    , hr [] []
    , Html.map StreamsChange (Streams.view model.streams)
    ]
