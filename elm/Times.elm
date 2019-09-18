module Times exposing (..)

import Time
import DateFormat exposing (..)
import Iso8601

omitSecond : String -> Time.Zone -> String
omitSecond s zone = case Iso8601.toTime s of
  Ok t ->  DateFormat.format [yearNumber
                              , text "/"
                              , monthNumber
                              , text "/" 
                              , dayOfMonthNumber
                              , text " "
                              , hourMilitaryFixed
                              , text ":"
                              , minuteFixed] zone t
  _ -> s