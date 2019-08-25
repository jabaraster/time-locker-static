module Api exposing (getCharacterList, getCharacterSummary, getTotalPlayState)

import Http
import Json.Decode as D
import Types exposing (..)


getCharacterList : (Result Http.Error CharacterList -> msg) -> Cmd msg
getCharacterList operation =
    Http.get
        { url = "/api/character/"
        , expect = Http.expectJson operation characterListDecoder
        }


getCharacterSummary : CharacterName -> (Result Http.Error CharacterSummary -> msg) -> Cmd msg
getCharacterSummary name operation =
    Http.get
        { url = "/api/character/" ++ name
        , expect = Http.expectJson operation characterSummaryDecoder
        }


getTotalPlayState : (Result Http.Error TotalPlayState -> msg) -> Cmd msg
getTotalPlayState operation =
    Http.get
        { url = "/api/total-play-state/"
        , expect = Http.expectJson operation totalPlayStateDecoder
        }
