module Generated.API exposing (Channel, Config, Message, User, getApiConfig, getApiMessages, jsonDecChannel, jsonDecConfig, jsonDecMessage, jsonDecUser, jsonEncChannel, jsonEncConfig, jsonEncMessage, jsonEncUser)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Url.Builder


type alias Config =
    { workspace : Maybe String
    , interval : Int
    }


jsonDecConfig : Json.Decode.Decoder Config
jsonDecConfig =
    Json.Decode.succeed (\pworkspace pinterval -> { workspace = pworkspace, interval = pinterval })
        |> fnullable "workspace" Json.Decode.string
        |> required "interval" Json.Decode.int


jsonEncConfig : Config -> Value
jsonEncConfig val =
    Json.Encode.object
        [ ( "workspace", maybeEncode Json.Encode.string val.workspace )
        , ( "interval", Json.Encode.int val.interval )
        ]


type alias User =
    { id : String
    , name : String
    , color : Maybe String
    }


jsonDecUser : Json.Decode.Decoder User
jsonDecUser =
    Json.Decode.succeed (\pid pname pcolor -> { id = pid, name = pname, color = pcolor })
        |> required "id" Json.Decode.string
        |> required "name" Json.Decode.string
        |> fnullable "color" Json.Decode.string


jsonEncUser : User -> Value
jsonEncUser val =
    Json.Encode.object
        [ ( "id", Json.Encode.string val.id )
        , ( "name", Json.Encode.string val.name )
        , ( "color", maybeEncode Json.Encode.string val.color )
        ]


type alias Channel =
    { id : String
    , name : String
    }


jsonDecChannel : Json.Decode.Decoder Channel
jsonDecChannel =
    Json.Decode.succeed (\pid pname -> { id = pid, name = pname })
        |> required "id" Json.Decode.string
        |> required "name" Json.Decode.string


jsonEncChannel : Channel -> Value
jsonEncChannel val =
    Json.Encode.object
        [ ( "id", Json.Encode.string val.id )
        , ( "name", Json.Encode.string val.name )
        ]


type alias Message =
    { user : User
    , text : String
    , channel : Channel
    , ts : String
    }


jsonDecMessage : Json.Decode.Decoder Message
jsonDecMessage =
    Json.Decode.succeed (\puser ptext pchannel pts -> { user = puser, text = ptext, channel = pchannel, ts = pts })
        |> required "user" jsonDecUser
        |> required "text" Json.Decode.string
        |> required "channel" jsonDecChannel
        |> required "ts" Json.Decode.string


jsonEncMessage : Message -> Value
jsonEncMessage val =
    Json.Encode.object
        [ ( "user", jsonEncUser val.user )
        , ( "text", Json.Encode.string val.text )
        , ( "channel", jsonEncChannel val.channel )
        , ( "ts", Json.Encode.string val.ts )
        ]


getApiMessages : (Result Http.Error (List Message) -> msg) -> Cmd msg
getApiMessages toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "messages"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecMessage)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getApiConfig : (Result Http.Error Config -> msg) -> Cmd msg
getApiConfig toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin ""
                [ "api"
                , "config"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg jsonDecConfig
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
