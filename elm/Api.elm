module Api exposing (getCharacterList, getCharacterResult, getDailyResult, getScoreRanking, getTotalResult)

import Http
import Json.Decode as D
import Time exposing (Zone)
import Types exposing (..)


getCharacterList : (Result Http.Error CharacterList -> msg) -> Cmd msg
getCharacterList operation =
    Http.get
        { url = "/api/character/"
        , expect = Http.expectJson operation characterListDecoder
        }


getCharacterResult : CharacterName -> (Result Http.Error CharacterResult -> msg) -> Cmd msg
getCharacterResult name operation =
    Http.get
        { url = "/api/character/" ++ name
        , expect = Http.expectJson operation characterResultDecoder
        }


getTotalResult : (Result Http.Error TotalResult -> msg) -> Cmd msg
getTotalResult operation =
    Http.get
        { url = "/api/total-result/"
        , expect = Http.expectJson operation totalResultDecoder
        }


getScoreRanking : (Result Http.Error ModePlayResults -> msg) -> Cmd msg
getScoreRanking operation =
    Http.get
        { url = "/api/score-ranking/"
        , expect = Http.expectJson operation modePlayResultsDecoder
        }


getDailyResult : (Result Http.Error DailyResultWork -> msg) -> Cmd msg
getDailyResult operation =
    Http.get
        { url = "/api/daily-result/"
        , expect = Http.expectJson operation dailyResultWorkDecoder
        }
