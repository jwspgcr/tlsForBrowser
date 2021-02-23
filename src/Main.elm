module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url



-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    hostName = "http://localhost:8000"
    urlString = String.replace hostName "" (Url.toString model.url)
  in
    case urlString of
      "/home" ->
        { title = "three-letterSiritori"
        , body =
            [viewHeader
            , a [href "/add"] [text "add"]
            ]
        }
      "/add" ->
        { title = "three-letterSiritori"
        , body =
            [viewHeader
            , text urlString
            ]
        }
      _ ->
        { title = "three-letterSiritori"
        , body =
            [ h1 [] [text "3文字しりとり"]
            , ul []
                [ viewLink "/home" "しりとり"
                , viewLink "/add" "単語の追加"
                , viewLink "/history" "履歴"
                , text urlString
                ]
            ]
        }


viewLink : String -> String -> Html msg
viewLink path txt =
  li [] [ a [ href path ] [ text txt ] ]

viewHeader : Html Msg
viewHeader = header []
  [ h1 [] [text "3文字しりとり"]
  , ul []
      [ viewLink "/home" "しりとら"
      , viewLink "/add" "単語の追加"
      , viewLink "/history" "履歴"
      ]
  ]
