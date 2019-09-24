module Times exposing (ZonedTime, dateString, omitSecond, parseDatetime)

import Date exposing (Date)
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


dateString : Date -> String
dateString =
    Date.format "yyyy/MM/dd"
