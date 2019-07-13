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
            , span [ class "message-info pl-2", style "color" "#616061" ]
                [ text (Maybe.withDefault "" <| TimeStamp.toDate model.zone msg.ts)
                , text (" in #" ++ msg.channel.name)
                ]
            ]
        , div [ class "message-content" ]
            [ pre [ class "f4" ] [ text msg.text ] ]
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
