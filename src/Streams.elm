module Streams exposing
  ( Model, init
  , view
  , update, Msg )

import Html exposing (div, table, td, text, Html, tr, h1, img)
import Html.App as Html
import Html.Attributes exposing (src)
import Http
import Json.Decode as Json exposing ( (:=) )
import Task


-- MODEL


type alias Channel =
  { status : String
  , displayName : String
  }


type alias Stream =
  { channel : Channel
  }


type alias Model =
  { game : String
  , streams : List Stream
  }


init : String -> (Model, Cmd Msg)
init game =
    ( Model game []
    , getStreams game )


-- UPDATE


type Msg
  = FetchSucceed (List Stream)
  | FetchFail Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed streams ->
      ( { model | streams = streams }
      , Cmd.none )
    FetchFail err ->
      ( model
      , Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  let
    fancyMakeRow : Stream -> Html Msg
    fancyMakeRow stream =
      tr [] [ td [] [ text stream.channel.displayName ]
            , td [] [ text stream.channel.status ]
            ]
    rows = model.streams |> List.map fancyMakeRow
  in
    div []
      [ table [] rows
      ]


-- NETWORK


getStreams : String -> Cmd Msg
getStreams game =
  let
    url = Http.url
      "https://api.twitch.tv/kraken/streams"
      [ ("game", game)
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
  Json.object1 Stream
    ("channel" := channelDecoder)


streamsDecoder : Json.Decoder (List Stream)
streamsDecoder =
  ("streams" := Json.list streamDecoder)
