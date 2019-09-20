module Times exposing (..)

import DateFormat exposing (..)
import Iso8601
import Time


parseDatetime : String -> Time.Posix
parseDatetime s =
    Result.withDefault (Time.millisToPosix 0) <| Iso8601.toTime s


omitSecond : Time.Posix -> Time.Zone -> String
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
