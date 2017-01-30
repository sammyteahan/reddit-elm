module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import Http
import Task

import Material
import Material.Scheme as Scheme
import Material.Layout as Layout
import Material.Button as Button
import Material.Color as Color
import Material.Options as Options exposing (css)
import Json.Decode.Pipeline as JsonPipeline exposing (decode, required)
import Json.Decode.Extra exposing ((|:))


-- Model(s)
type alias Post =
  { title : String
  , url : String
  , domain : String
  }

type alias Model =
  { searchString : String
  , fetching : Bool
  , imgUrl : String
  , posts : List Post
  , mdl : Material.Model
  }

init : (Model, Cmd Msg)
init =
    ( Model "elm" True "loading.gif" [] Material.model
    , getSubReddit "elm"
    )


-- Update
type Msg
  = UpdateSearchString String
  | GetSubreddit
  | NewSubreddit (Result Http.Error (List Post))
  | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSearchString newString ->
      { model | searchString = newString } ! []

    GetSubreddit ->
      { model | fetching = True } ! [ getSubReddit model.searchString ]

    NewSubreddit (Ok posts) ->
      { model | posts = posts, fetching = False } ! []

    NewSubreddit (Err _) ->
      (model, Cmd.none)

    Mdl msg_ ->
      Material.update Mdl msg_ model


-- View
view : Model -> Html Msg
view model =
  Scheme.topWithScheme Color.Red Color.Amber -- Don't do this 'top' thing in prod. This 'injects' google css to top of script
    <| Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      ]
      { header = [ h1 [ style [("padding-left", "20px")]] [ text "Elm Reddit" ] ]
      , drawer = []
      , tabs = ( [], [] )
      , main = [ viewContent model ]
      }

viewContent : Model -> Html Msg
viewContent model =
  div [ class "container" ]
    [ input [ placeholder "Subreddit"
            , autofocus True
            , onInput UpdateSearchString
            ] []
    , Button.render Mdl [ 0 ] model.mdl [ Options.onClick GetSubreddit ] [ text "Get info" ]
    , br [] []
    , h2 [] [ text model.searchString ]
    , div [ class "wrap-posts" ]
      [ h2 [] [ text "Posts" ]
      , br [] []
      , section []
        [ ul [ class "posts" ]
          (List.map postView model.posts)
        ]
      ]
    ]
    -- Scheme.topWithScheme Color.Teal Color.Green

postView : Post -> Html Msg
postView post =
  div []
    [ a [ style [("color", "#000")], href post.url, target "_blank" ] [ text post.title ] ]


-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- Http
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


-- Main
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
