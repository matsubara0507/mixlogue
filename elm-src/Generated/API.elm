module Generated.API exposing (Channel, Config, Message, User, decodeChannel, decodeConfig, decodeMessage, decodeUser, encodeChannel, encodeConfig, encodeMessage, encodeUser, getApiConfig, getApiMessages)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import String


type alias Config =
    { workspace : Maybe String
    , interval : Int
    }


decodeConfig : Decoder Config
decodeConfig =
    Json.Decode.succeed Config
        |> required "workspace" (maybe string)
        |> required "interval" int


encodeConfig : Config -> Json.Encode.Value
encodeConfig x =
    Json.Encode.object
        [ ( "workspace", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.workspace )
        , ( "interval", Json.Encode.int x.interval )
        ]


type alias User =
    { id : String
    , name : String
    , color : String
    }


decodeUser : Decoder User
decodeUser =
    Json.Decode.succeed User
        |> required "id" string
        |> required "name" string
        |> required "color" string


encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "color", Json.Encode.string x.color )
        ]


type alias Channel =
    { id : String
    , name : String
    }


decodeChannel : Decoder Channel
decodeChannel =
    Json.Decode.succeed Channel
        |> required "id" string
        |> required "name" string


encodeChannel : Channel -> Json.Encode.Value
encodeChannel x =
    Json.Encode.object
        [ ( "id", Json.Encode.string x.id )
        , ( "name", Json.Encode.string x.name )
        ]


type alias Message =
    { user : User
    , text : String
    , channel : Channel
    , ts : String
    }


decodeMessage : Decoder Message
decodeMessage =
    Json.Decode.succeed Message
        |> required "user" decodeUser
        |> required "text" string
        |> required "channel" decodeChannel
        |> required "ts" string


encodeMessage : Message -> Json.Encode.Value
encodeMessage x =
    Json.Encode.object
        [ ( "user", encodeUser x.user )
        , ( "text", Json.Encode.string x.text )
        , ( "channel", encodeChannel x.channel )
        , ( "ts", Json.Encode.string x.ts )
        ]


getApiMessages : Http.Request (List Message)
getApiMessages =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "messages"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeMessage)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getApiConfig : Http.Request Config
getApiConfig =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "config"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeConfig
        , timeout =
            Nothing
        , withCredentials =
            False
        }
