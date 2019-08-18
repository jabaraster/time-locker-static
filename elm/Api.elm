module Api exposing (getCharacterList, getCharacterSummary)

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
