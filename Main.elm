module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import Http
import Task

import Material
import Material.Scheme as Scheme
import Material.List as Lists
import Material.Layout as Layout
import Material.Button as Button
import Material.Color as Color
import Material.Card as Card
import Material.Textfield as Textfield
import Material.Options as Options exposing (css)
import Json.Decode.Pipeline as JsonPipeline exposing (decode, required, requiredAt)
import Json.Decode.Extra exposing ((|:))


-- Model(s)
type alias Post =
  { title : String
  , url : String
  , domain : String
  }

type alias Model =
  { searchString : String
  , selectedReddit : String
  , fetching : Bool
  , fetchError : String
  , imgUrl : String
  , posts : List Post
  , mdl : Material.Model
  }

model : Model
model =
  { searchString = ""
  , selectedReddit = "elm"
  , fetching = True
  , fetchError = ""
  , imgUrl = "loading.gif"
  , posts = []
  , mdl = Material.model
  }

init : (Model, Cmd Msg)
init =
    ( model, getSubReddit "elm")


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
      { model | fetching = True, selectedReddit = model.searchString }
      ! [ getSubReddit model.searchString ]

    NewSubreddit (Ok posts) ->
      { model | posts = posts, fetching = False } ! []

    NewSubreddit (Err _) ->
      let
        errorMessage =
          "Some went wrong 😳"
      in
        { model | searchString = errorMessage, fetching = False } ! []

    Mdl action_ ->
      Material.update Mdl action_ model


-- View
-- Don't use 'Schem.top..' in prod. It does handle some style/Scheme
-- related stuff, but all it really does is inject google stylesheets
-- into a header script, which should be done in index.html
view : Model -> Html Msg
view model =
  Scheme.topWithScheme Color.Teal Color.LightGreen
    <| Layout.render Mdl
      model.mdl
      [ Layout.fixedHeader
      ]
      { header = [ header ]
      , drawer = []
      , tabs = ( [], [] )
      , main = [ viewContent model ]
      }

-- TODO render menu and search bar
header : Html Msg
header =
  div []
    [ h4 [ style [("float", "left"), ("padding-left", "20px")]] [ text "Reddit" ]
    ]


containerStyle : List (Options.Property a b)
containerStyle =
    [ css "margin" "auto"
    , css "padding-left" "8%"
    , css "padding-right" "8%"
    , css "padding-top" "25px"
    ]

viewContent : Model -> Html Msg
viewContent model =
  Options.div containerStyle
    [ Textfield.render Mdl [1] model.mdl
      [ Options.onInput UpdateSearchString
      ]
      []
    , Button.render Mdl [2] model.mdl
      [ Button.raised
      , Button.ripple
      , Button.colored
      , Options.onClick GetSubreddit
      , css "margin-left" "10px"
      ]
      [ text "Fetch Subreddit" ]
    , br [] []
    , h2 [] [ text model.selectedReddit ]
    , div [ class "wrap-posts" ]
      [ section []
        [ listContent model ]
      ]
    ]

listContent : Model -> Html Msg
listContent model =
  let
    renderContent =
      if model.fetching then
        div [] [ text "loading..." ]
      else
        div []
          [ Lists.ul []
            (List.map listView model.posts)
          ]
  in
    renderContent

listView : Post -> Html Msg
listView post =
  Lists.li []
    [ Lists.content []
      [ a [ style [ ("color", "rgba(0, 0, 0, 0.87)")
                  , ("text-decoration", "none")
                  , ("font-weight", "400")
                  ]
          , href post.url
          , target "_blank"
          ]
          [ text post.title ]
        ]
    ]

loadingView : Model -> Html Msg
loadingView model =
  div []
    [ text "loading" ]


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
  decode Post
    |> requiredAt ["data", "title"] Json.string
    |> requiredAt ["data", "url"] Json.string
    |> requiredAt ["data", "domain"] Json.string

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
