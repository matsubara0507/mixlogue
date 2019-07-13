module TimeStamp exposing (TimeStamp, dayToString, format, monthToString, toDate, toMilis, withoutDot)

import Time
import Time.Extra as Time


type alias TimeStamp =
    String


toDate : Time.Zone -> TimeStamp -> Maybe String
toDate zone ts =
    toMilis ts
        |> Maybe.map Time.millisToPosix
        |> Maybe.map (Time.posixToParts zone)
        |> Maybe.map format


toMilis : TimeStamp -> Maybe Int
toMilis ts =
    String.split "." ts
        |> List.head
        |> Maybe.andThen String.toInt
        |> Maybe.map (\n -> n * 1000)


format : Time.Parts -> String
format parts =
    String.concat
        [ monthToString parts.month
        , " "
        , dayToString parts.day
        , " at "
        , String.right 2 ("0" ++ String.fromInt parts.hour)
        , ":"
        , String.right 2 ("0" ++ String.fromInt parts.minute)
        ]


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


dayToString : Int -> String
dayToString day =
    case ( 0 < day && day < 32, day, modBy 10 day ) of
        ( False, _, _ ) ->
            String.fromInt day

        ( _, 11, _ ) ->
            "11th"

        ( _, 12, _ ) ->
            "12th"

        ( _, _, 1 ) ->
            String.fromInt day ++ "st"

        ( _, _, 2 ) ->
            String.fromInt day ++ "nd"

        ( _, _, 3 ) ->
            String.fromInt day ++ "rd"

        ( _, _, _ ) ->
            String.fromInt day ++ "th"


withoutDot : TimeStamp -> TimeStamp
withoutDot ts =
    String.split "." ts |> String.concat
