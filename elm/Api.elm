module Api exposing (characterDecoder, getCharacterList)

import Http
import Json.Decode as D
import Types


getCharacterList : (Result Http.Error (List Types.Character) -> msg) -> Cmd msg
getCharacterList operation =
    Http.get
        { url = "/api/character/"
        , expect = Http.expectJson operation (D.list characterDecoder)
        }


characterDecoder : D.Decoder Types.Character
characterDecoder =
    D.map Types.Character (D.field "name" D.string)
