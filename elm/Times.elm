module Times exposing (..)

import DateFormat exposing (..)
import Iso8601
import Time exposing (Posix, Zone)


type alias ZonedTime =
    { time : Posix
    , zone : Zone
    }


parseDatetime : String -> Posix
parseDatetime s =
    Result.withDefault (Time.millisToPosix 0) <| Iso8601.toTime s


omitHour : Posix -> Zone -> String
omitHour t zone =
    DateFormat.format
        [ yearNumber
        , text "/"
        , monthNumber
        , text "/"
        , dayOfMonthNumber
        ]
        zone
        t


omitHour2 : ZonedTime -> String
omitHour2 t =
    omitHour t.time t.zone


omitSecond : Posix -> Zone -> String
omitSecond t zone =
    DateFormat.format
        [ yearNumber
        , text "/"
        , monthNumber
        , text "/"
        , dayOfMonthNumber
        , text " "
        , hourMilitaryFixed
        , text ":"
        , minuteFixed
        ]
        zone
        t
