module Types exposing (Character, ScoreData, characterDecoder, scoreDataDecoder)

import Json.Decode as D


type alias Character =
    { name : String
    , normal : Maybe ScoreData
    , hard : Maybe ScoreData
    }


type alias ScoreData =
    { playCount : Int
    , highScore : Int
    , averageScore : Float
    }


characterDecoder : D.Decoder Character
characterDecoder =
    D.map3 Character
        (D.field "name" D.string)
        (D.maybe <| D.field "normal" scoreDataDecoder)
        (D.maybe <| D.field "hard" scoreDataDecoder)


scoreDataDecoder : D.Decoder ScoreData
scoreDataDecoder =
    D.map3 ScoreData
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)
