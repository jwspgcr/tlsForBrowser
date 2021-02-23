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
    let
        postRequest : (Result Http.Error String -> Msg) -> Request -> Cmd Msg
        postRequest message req =
            Http.post
                { url = "https://jwspgcrtls2.pythonanywhere.com"
                , body = Http.stringBody "application/x-www-form-urlencoded" (newRequest req)
                , expect = Http.expectJson message textDecoder
                }

        textDecoder : D.Decoder String
        textDecoder =
            D.field "text" D.string

        newRequest : Request -> String
        newRequest req =
            [ "user_id=", req.userID, "&command=", req.command, "&text", req.text ] |> String.concat

        showResult : Result Http.Error String -> (String -> Model) -> ( Model, Cmd Msg )
        showResult result modelFunc =
            case result of
                Ok message ->
                    ( modelFunc message, Cmd.none )

                Err error ->
                    case error of
                        Http.BadUrl message ->
                            ( modelFunc ("BadUrl" ++ message), Cmd.none )

                        Http.Timeout ->
                            ( modelFunc "Timeout", Cmd.none )

                        Http.NetworkError ->
                            ( modelFunc "NetworkError", Cmd.none )

                        Http.BadStatus code ->
                            ( modelFunc ("BadStatus" ++ String.fromInt code), Cmd.none )

                        Http.BadBody message ->
                            ( modelFunc ("BadBody" ++ message), Cmd.none )
    in
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
            showResult result (\text -> { model | userText = text })

        GotAtHome result ->
            showResult result (\text -> { model | homeText = text })

        GotAtAdd result ->
            showResult result (\text -> { model | addText = text })

        GotAtHistory result ->
            showResult result (\text -> { model | historyText = text })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.url.path of
        "/tlsForBrowser/home" ->
            viewDocument
                [ viewHeader model
                , input [ placeholder "三文字のかたかな", value model.inputText, onInput ChangeAtHome ] []
                , button [ onClick PostAtHome ] [ text "送信" ]
                , text model.homeText
                ]

        "/tlsForBrowser/add" ->
            viewDocument
                [ viewHeader model
                , input [ placeholder "三文字のかたかな", value model.inputText, onInput ChangeAtAdd ] []
                , button [ onClick PostAtAdd ] [ text "送信" ]
                , text model.addText
                ]

        "/tlsForBrowser/history" ->
            viewDocument
                [ viewHeader model
                , button [ onClick PostAtHistory ] [ text "履歴取得" ]
                , text model.historyText
                ]

        _ ->
            viewDocument
                [ viewHeader model
                , button [ onClick PostAtHistory ] [ text "履歴取得" ]
                , text "このページは存在しません。"
                ]


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


viewDocument : List (Html Msg) -> Browser.Document Msg
viewDocument msgHtmlList =
    { title = "three-letterSiritori"
    , body = msgHtmlList
    }



--TYPE


type alias Request =
    { userID : String
    , command : String
    , text : String
    }
