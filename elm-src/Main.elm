module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style)
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
    , zone : Time.Zone
    }


type Msg
    = Fresh
    | Tick Posix
    | FetchZone Time.Zone
    | FetchMessages (Result Http.Error (List API.Message))


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { messages = []
            , interval = 5
            , zone = Time.utc
            }
    in
    ( model, Cmd.batch [ Task.perform FetchZone Time.here, fetchMessages ] )


view : Model -> Html Msg
view model =
    div [] (List.map (viewMessage model) model.messages)


viewMessage : Model -> API.Message -> Html Msg
viewMessage model msg =
    div [ class "message container-lg clearfix" ]
        [ div [ class "message-info col-2 float-left p-2" ]
            [ div [ class "message-date" ]
                [ text (Maybe.withDefault "" <| TimeStamp.toDate model.zone msg.ts) ]
            , div [ class "message-channel pl-1" ]
                [ text (" in #" ++ msg.channel.name) ]
            ]
        , div [ class "message-body col-6 float-left p-2" ]
            [ div [ class "message-user", style "color" msg.user.color ]
                [ text msg.user.name ]
            , pre [ class "message-content pl-1" ]
                [ text msg.text ]
            ]
        ]


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


fetchMessages : Cmd Msg
fetchMessages =
    Http.send FetchMessages API.getApiMessages


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.interval Tick
