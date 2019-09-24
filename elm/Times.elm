module Times exposing (ZonedTime, omitHour, omitHour2, omitSecond, parseDatetime, toHourOmittedTime)

import DateFormat exposing (..)
import Iso8601
import Time exposing (Posix, Zone)
import Time.Extra as TE


type alias ZonedTime =
    { time : Posix
    , zone : Zone
    }


parseDatetime : String -> Posix
parseDatetime s =
    Result.withDefault (Time.millisToPosix 0) <| Iso8601.toTime s


omitHour : Zone -> Posix -> String
omitHour zone t =
    DateFormat.format
        [ yearNumber
        , text "/"
        , monthFixed
        , text "/"
        , dayOfMonthFixed
        ]
        zone
        t


omitHour2 : ZonedTime -> String
omitHour2 t =
    omitHour t.zone t.time


toHourOmittedTime : ZonedTime -> Posix
toHourOmittedTime t =
    TE.floor TE.Day t.zone t.time


omitSecond : Zone -> Posix -> String
omitSecond zone t =
    DateFormat.format
        [ yearNumber
        , text "/"
        , monthFixed
        , text "/"
        , dayOfMonthFixed
        , text " "
        , hourMilitaryFixed
        , text ":"
        , minuteFixed
        ]
        zone
        t
