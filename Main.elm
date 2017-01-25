import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing (string)
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

-- Model
type alias Model =
  { searchString : String
  }

init : (Model, Cmd Msg)
init =
    ( Model "allthingsprotoss"
    , Cmd.none
    )

-- Update
type Msg
  = UpdateSearchString String
  -- | NewSubreddit (Result Http.Error Posts)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSearchString newString ->
      { model | searchString = newString } ! []

    -- NewSubreddit (Ok posts) ->
    --   (model, Cmd.none)
    --
    -- NewSubreddit (Err _) ->
    --   (model, Cmd.none)

-- View
view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Subreddit"
            , autofocus True
            , onInput UpdateSearchString
            ] []
    , button [] [ text "Get info" ]
    , br [] []
    , h2 [] [ text model.searchString ]
    ]

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Http
-- type alias Post =
--   { title : String
--   , url : String
--   }
--
-- type alias Posts = List Post
--
-- getSubReddit : String -> Cmd Msg
-- getSubReddit searchString =
--   let
--     url = "https://www.reddit.com/r/" ++ searchString
--   in
--     Http.send NewSubreddit (Http.get url decodePosts)
--
-- decodePosts : Json.Decoder Posts
-- decodePosts =
--   decode Posts
--     |> JsonPipeline.required "data" string
--
-- decodePost : Json.Decoder Post
-- decodePost =
--   decode Post
--     |> JsonPipeline.required "title" string
--     |> JsonPipeline.required "url" string
