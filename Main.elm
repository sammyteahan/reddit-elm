module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http

import Json.Decode as Json exposing (..)
import Json.Decode.Pipeline as JsonPipeline exposing (decode, required)
import Json.Decode.Extra exposing ((|:))
import Task


-- Main
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- Model(s)
type alias Post =
  { title : String
  , url : String
  , domain : String
  }

type alias Model =
  { searchString : String
  , posts : List Post
  }

init : (Model, Cmd Msg)
init =
    ( Model "reactjs" []
    , Cmd.none
    )


-- Update
type Msg
  = UpdateSearchString String
  | GetSubreddit
  | NewSubreddit (Result Http.Error (List Post))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSearchString newString ->
      { model | searchString = newString } ! []

    GetSubreddit ->
      { model | searchString = "rails" } ! [ getSubReddit model.searchString ]

    NewSubreddit (Ok posts) ->
      (model, Cmd.none)

    NewSubreddit (Err _) ->
      (model, Cmd.none)


-- View
view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Subreddit"
            , autofocus True
            , onInput UpdateSearchString
            ] []
    , button [ onClick GetSubreddit ] [ text "Get info" ]
    , br [] []
    , h2 [] [ text model.searchString ]
    ]


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- Http

-- {
--   "data": {
--     "children": [
--       {"data": {"url": "something.com", "title": "some title"}},
--       {"data": {"url": "another-something.com", "title": "another title"}}
--     ]
--   }
-- }

getSubReddit : String -> Cmd Msg
getSubReddit searchString =
  let
    url = "https://www.reddit.com/r/" ++ searchString ++ ".json"
  in
    Http.send NewSubreddit (Http.get url decodePosts)

decodePost : Json.Decoder Post
decodePost =
  Json.map3 Post
    (Json.at ["data", "title"] Json.string)
    (Json.at ["data", "url"] Json.string)
    (Json.at ["data", "domain"] Json.string)

decodePosts : Json.Decoder (List Post)
decodePosts =
  Json.list decodePost
    |> Json.at ["data", "children"]
