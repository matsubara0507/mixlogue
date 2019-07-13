module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Http
import Task
import Time exposing (Posix)
import TimeStamp


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { messages : List API.Message
    , interval : Float
    , workspace : Maybe String
    , zone : Time.Zone
    }


type Msg
    = Fresh
    | Tick Posix
    | FetchZone Time.Zone
    | FetchMessages (Result Http.Error (List API.Message))
    | FetchConfig (Result Http.Error API.Config)


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { messages = []
            , interval = 5
            , workspace = Nothing
            , zone = Time.utc
            }
    in
    ( model
    , Cmd.batch
        [ fetchConfig
        , Task.perform FetchZone Time.here
        , fetchMessages
        ]
    )


view : Model -> Html Msg
view model =
    div [ class "container-md p-3" ]
        [ div [ class "Box" ]
            [ div [ class "Box-header" ]
                [ h3 [] [ text "Mixlogue" ] ]
            , div [] (List.map (viewMessage model) model.messages)
            ]
        ]


viewMessage : Model -> API.Message -> Html Msg
viewMessage model msg =
    div [ class "message Box-row" ]
        [ div [ class "message-header" ]
            [ h4 [ class "message-user d-inline", style "color" msg.user.color ]
                [ text msg.user.name ]
            , viewMessageInfo model msg
            ]
        , div [ class "message-content" ]
            [ pre [ class "f4" ] [ text msg.text ] ]
        ]


viewMessageInfo : Model -> API.Message -> Html Msg
viewMessageInfo model msg =
    let
        info =
            String.concat
                [ Maybe.withDefault "" (TimeStamp.toDate model.zone msg.ts)
                , " in #"
                , msg.channel.name
                ]

        attrs =
            [ class "message-info pl-2", style "color" "#616061" ]
    in
    case model.workspace of
        Just domain ->
            let
                url =
                    String.concat
                        [ "https://"
                        , domain
                        , ".slack.com/archives/"
                        , msg.channel.id
                        , "/p"
                        , TimeStamp.withoutDot msg.ts
                        ]
            in
            a ([ href url ] ++ attrs) [ text info ]

        Nothing ->
            span attrs [ text info ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fresh ->
            ( model, fetchMessages )

        Tick _ ->
            ( model, fetchMessages )

        FetchZone zone ->
            ( { model | zone = zone }, Cmd.none )

        FetchMessages (Ok messages) ->
            ( { model | messages = messages }, Cmd.none )

        FetchMessages (Err _) ->
            ( model, Cmd.none )

        FetchConfig (Ok conf) ->
            ( { model
                | interval = toFloat conf.interval
                , workspace = conf.workspace
              }
            , Cmd.none
            )

        FetchConfig (Err _) ->
            ( model, Cmd.none )


fetchMessages : Cmd Msg
fetchMessages =
    Http.send FetchMessages API.getApiMessages


fetchConfig : Cmd Msg
fetchConfig =
    Http.send FetchConfig API.getApiConfig


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.interval Tick
