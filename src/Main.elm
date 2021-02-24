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
    , outputText : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url "" "" "ここにサーバーからの応答が表示されます。", Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PostAtUser
    | PostAtHome
    | PostAtAdd
    | PostAtHistory
    | PostAtHelp
    | GotText (Result Http.Error String)
    | ChangeAtUser String
    | ChangeAtHome String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        postRequest : (Result Http.Error String -> Msg) -> Request -> Cmd Msg
        postRequest message req =
            Http.post
                -- { url = "https://jwspgcrtls2.pythonanywhere.com"
                { url = "http://127.0.0.1:8000"
                , body = Http.stringBody "application/x-www-form-urlencoded" (newRequest req)
                , expect = Http.expectJson message textDecoder
                }

        textDecoder : D.Decoder String
        textDecoder =
            D.field "text" D.string

        newRequest : Request -> String
        newRequest req =
            [ "user_id=", req.userID, "&command=", req.command, "&text=", req.text ] |> String.concat

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

        PostAtUser ->
            ( model, postRequest GotText (Request model.inputUser "/tls-init" "") )

        PostAtHome ->
            ( model, postRequest GotText (Request model.inputUser "/tls" model.inputText) )

        PostAtAdd ->
            ( model, postRequest GotText (Request model.inputUser "/tls-add" model.inputText) )

        PostAtHistory ->
            ( model, postRequest GotText (Request model.inputUser "/tls-history" "") )

        PostAtHelp ->
          ( model, postRequest GotText (Request "" "/tls-help" "") )

        GotText result ->
            showResult result (\text -> { model | outputText = text })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.url.path of
        "/tlsForBrowser/" ->
          viewDocument
              [ viewHeader model
              , input [ placeholder "三文字のかたかな", value model.inputText, onInput ChangeAtHome ] []
              , button [ onClick PostAtHome
                      , enableIfUserIdExists model
                      ] [ text "/tls" ]
              , button [ onClick PostAtAdd
                      , enableIfUserIdExists model
                      ] [ text "/tls-add" ]
              , button [ onClick PostAtHistory
                      , enableIfUserIdExists model
                      ] [ text "/tls-history" ]
              , br [] []
              , pre [] [ text model.outputText ]
              ]

        _ ->
            viewDocument
                [ viewHeader model
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
            [ button [ onClick PostAtHelp] [ text "/tls-help" ]
            , br [] []
            , input
              [ placeholder "お名前"
              , value model.inputUser
              , onInput ChangeAtUser
              ] []
            , button [ onClick PostAtUser
                    , enableIfUserIdExists model] [ text "/tls-init" ]
            , hr [] []
            ]
        ]


viewDocument : List (Html Msg) -> Browser.Document Msg
viewDocument msgHtmlList =
    { title = "three-letterSiritori"
    , body = msgHtmlList
    }


enableIfUserIdExists: Model -> Html.Attribute Msg
enableIfUserIdExists model =
    if  String.length model.inputUser  == 0 then
      disabled True
    else
      disabled False



--TYPE


type alias Request =
    { userID : String
    , command : String
    , text : String
    }
