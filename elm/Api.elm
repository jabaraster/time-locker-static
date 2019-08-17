module Api exposing (getCharacterList)

import Http
import Json.Decode as D
import Types


getCharacterList : (Result Http.Error Types.CharacterList -> msg) -> Cmd msg
getCharacterList operation =
    Http.get
        { url = "/api/character/"
        , expect = Http.expectJson operation Types.characterListDecoder
        }
