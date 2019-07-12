module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser as Browser
import Generated.API as API exposing (..)
import Html exposing (..)
import Http
import Time exposing (Posix)


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
    }


type Msg
    = Fresh
    | Tick Posix
    | FetchMessages (Result Http.Error (List API.Message))


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { messages = []
            , interval = 5
            }
    in
    ( model, Cmd.batch [ fetchMessages ] )


view : Model -> Html Msg
view model =
    div [] (List.map viewMessage model.messages)


viewMessage : API.Message -> Html Msg
viewMessage msg =
    div [] [ text (Debug.toString msg) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fresh ->
            ( model, fetchMessages )

        Tick _ ->
            ( model, fetchMessages )

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
