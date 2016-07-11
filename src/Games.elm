module Games exposing
  ( Model, init
  , view
  , update, Msg(SelectGame) )

import Html exposing (Html, div, p, text, select, option)
import Html.App as Html
import Html.Attributes exposing (selected)
import Html.Events
import Http
import Json.Decode as Json exposing ( (:=) )
import Task
import String


-- MODEL


type alias Model =
  { selectedGame : String
  , possibleGames : List String
  }


init : String -> (Model, Cmd Msg)
init game =
    ( Model game []
    , getGames )


allGames : Model -> (List String)
allGames model =
  if List.member model.selectedGame model.possibleGames then
    model.possibleGames
  else
    [model.selectedGame] ++ model.possibleGames


-- UPDATE


type Msg
  = LoadedGames (List String)
  | FetchFail Http.Error
  | SelectGame String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadedGames games ->
      ( { model | possibleGames = games }
      , Cmd.none )
    FetchFail err ->
      ( model
      , Cmd.none )
    SelectGame newGame ->
      ( { model | selectedGame = newGame }
      , Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  let
    onSelect : (String -> msg) -> Html.Attribute msg
    onSelect tagger = Html.Events.on
      "change"
      (Json.map tagger Html.Events.targetValue)
  in
    div []
      [ select [ onSelect SelectGame ]
          (model |> allGames |> List.map (gameOption model.selectedGame))
      ]


gameOption : String -> String -> Html Msg
gameOption selectedGame game =
  if String.toLower game == selectedGame then
    option [ selected True ] [ text game ]
  else
    option [] [ text game ]


-- NETWORK


getGames : Cmd Msg
getGames =
  let
    url = Http.url "https://api.twitch.tv/kraken/games/top" []
  in
    Task.perform FetchFail LoadedGames (Http.get gamesDecoder url)


gamesDecoder : Json.Decoder (List String)
gamesDecoder =
  ("top" := Json.list ("game" := ("name" := Json.string)))
