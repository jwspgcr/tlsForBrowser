module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
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
    , inputUser : String
    , inputText : String
    , userText : String
    , homeText : String
    , historyText : String
    , addText : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url "" "" "" "" "" "", Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PostAtUser
    | PostAtHome
    | PostAtAdd
    | PostAtHistory
    | GotAtUser (Result Http.Error String)
    | GotAtHome (Result Http.Error String)
    | GotAtAdd (Result Http.Error String)
    | GotAtHistory (Result Http.Error String)
    | ChangeAtUser String
    | ChangeAtHome String
    | ChangeAtAdd String


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

        ChangeAtUser text ->
            ( { model | inputUser = text }, Cmd.none )

        ChangeAtHome text ->
            ( { model | inputText = text }, Cmd.none )

        ChangeAtAdd text ->
            ( { model | inputText = text }, Cmd.none )

        PostAtUser ->
            ( model, postRequest GotAtUser (Request model.inputUser "/tls-init" "") )

        PostAtHome ->
            ( model, postRequest GotAtHome (Request model.inputUser "/tls" model.inputText) )

        PostAtAdd ->
            ( model, postRequest GotAtAdd (Request model.inputUser "/tls-add" model.inputText) )

        PostAtHistory ->
            ( model, postRequest GotAtHistory (Request model.inputUser "/tls-history" "") )

        GotAtUser result ->
            case result of
                Ok message ->
                    ( { model | userText = message }, Cmd.none )

                Err _ ->
                    ( { model | userText = "レスポンスの取得に失敗しました。" }, Cmd.none )

        GotAtHome result ->
            case result of
                Ok message ->
                    ( { model | homeText = message }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadUrl message ->
                            ( { model | homeText = "BadUrl" ++ message }, Cmd.none )

                        Http.Timeout ->
                            ( { model | homeText = "Timeout" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | homeText = "NetworkError" }, Cmd.none )

                        Http.BadStatus code ->
                            ( { model | homeText = "BadStatus" ++ String.fromInt code }, Cmd.none )

                        Http.BadBody message ->
                            ( { model | homeText = "BadBody" ++ message }, Cmd.none )

        GotAtAdd result ->
            case result of
                Ok message ->
                    ( { model | addText = message }, Cmd.none )

                Err _ ->
                    ( { model | addText = "レスポンスの取得に失敗しました。" }, Cmd.none )

        GotAtHistory result ->
            case result of
                Ok message ->
                    ( { model | historyText = message }, Cmd.none )

                Err _ ->
                    ( { model | historyText = "レスポンスの取得に失敗しました。" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.url.path of
        "/tlsForBrowser/home" ->
            { title = "three-letterSiritori"
            , body =
                [ viewHeader model
                , input [ placeholder "三文字のかたかな", value model.inputText, onInput ChangeAtHome ] []
                , button [ onClick PostAtHome ] [ text "送信" ]
                , text model.homeText
                ]
            }

        "/tlsForBrowser/add" ->
            { title = "three-letterSiritori"
            , body =
                [ viewHeader model
                , input [ placeholder "三文字のかたかな", value model.inputText, onInput ChangeAtAdd ] []
                , button [ onClick PostAtAdd ] [ text "送信" ]
                , text model.addText
                ]
            }

        "/tlsForBrowser/history" ->
            { title = "three-letterSiritori"
            , body =
                [ viewHeader model
                , button [ onClick PostAtHistory ] [ text "履歴取得" ]
                , text model.historyText
                ]
            }

        _ ->
            { title = "three-letterSiritori"
            , body =
                [ viewHeader model
                , text "このページは存在しません。"
                ]
            }


viewLink : String -> String -> Html msg
viewLink path txt =
    li [] [ a [ href path ] [ text txt ] ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ h1 [] [ text "三文字しりとり" ]
        , ul []
            [ viewLink "/tlsForBrowser/home" "しりとり"
            , viewLink "/tlsForBrowser/add" "単語の追加"
            , viewLink "/tlsForBrowser/history" "履歴"
            , input [ placeholder "お名前", value model.inputUser, onInput ChangeAtUser ] []
            , text model.userText
            , button [ onClick PostAtUser ] [ text "しりとりを初期化" ]
            ]
        ]



-- HTTP

postRequest :((Result Http.Error String) -> Msg) ->Request -> Cmd Msg
postRequest msg req=
  Http.post
      { url = "https://jwspgcrtls2.pythonanywhere.com"
      , body = Http.stringBody "application/x-www-form-urlencoded" (newRequest req)
      , expect = Http.expectJson msg textDecoder
      }

postAtUser msg req =
  postCommon GotAtUser req


postAtHome : String -> String -> Cmd Msg
postAtHome =
    Http.post
        { url = "https://jwspgcrtls2.pythonanywhere.com"
        , body = Http.stringBody "application/x-www-form-urlencoded" newRequest
        , expect = Http.expectJson GotAtHome textDecoder
        }


postAtAdd : String -> String -> Cmd Msg
postAtAdd userID jsonText =
    Http.post
        { url = "https://jwspgcrtls2.pythonanywhere.com"
        , body = Http.jsonBody (textEncoder userID "/tls-add" jsonText)
        , expect = Http.expectJson GotAtAdd textDecoder
        }


postAtHistory : String -> Cmd Msg
postAtHistory userID =
    Http.post
        { url = "https://jwspgcrtls2.pythonanywhere.com"
        , body = Http.jsonBody (textEncoder userID "/tls-history" "")
        , expect = Http.expectJson GotAtHistory textDecoder
        }


textDecoder : D.Decoder String
textDecoder =
    D.field "text" D.string


newRequest : Request -> E.Value
newRequest req =
    [ "user_id=", req.userID, "&command=", req.command, "&text", req, text ] |> String.concat



-- TYPE


type alias Request =
    { userID : String
    , command : String
    , text : String
    }
