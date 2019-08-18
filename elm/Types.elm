module Types exposing (Armament, CharacterList, CharacterListElement, CharacterName, CharacterSummary, CharacterSummaryElement, PlayResult, ScoreData, armamentDecoder, characterListDecoder, characterListElementDecoder, characterSummaryDecoder, characterSummaryElementDecoder, emptyCharacterSummary, playResultDecoder, scoreDataDecoder)

import Json.Decode as D


type alias CharacterName =
    String


type alias CharacterListElement =
    { character : CharacterName
    , normal : Maybe ScoreData
    , hard : Maybe ScoreData
    }


characterListElementDecoder : D.Decoder CharacterListElement
characterListElementDecoder =
    D.map3 CharacterListElement
        (D.field "character" D.string)
        (D.maybe <| D.field "normal" scoreDataDecoder)
        (D.maybe <| D.field "hard" scoreDataDecoder)


type alias CharacterList =
    List CharacterListElement


characterListDecoder : D.Decoder CharacterList
characterListDecoder =
    D.list characterListElementDecoder


type alias Armament =
    { name : String
    , level : Int
    }


armamentDecoder : D.Decoder Armament
armamentDecoder =
    D.map2 Armament
        (D.field "name" D.string)
        (D.field "level" D.int)


type alias ScoreData =
    { playCount : Int
    , highScore : Int
    , averageScore : Float
    }


scoreDataDecoder : D.Decoder ScoreData
scoreDataDecoder =
    D.map3 ScoreData
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)


type alias PlayResult =
    { score : Int
    , armaments : List Armament
    , reasons : List String
    , playTime : String
    }


playResultDecoder : D.Decoder PlayResult
playResultDecoder =
    D.map4 PlayResult
        (D.field "score" D.int)
        (D.field "armaments" <| D.list armamentDecoder)
        (D.field "reasons" <| D.list D.string)
        (D.field "created" D.string)


type alias CharacterSummaryElement =
    { scoreSummary : ScoreData
    , scoreRanking : List PlayResult
    }


characterSummaryElementDecoder : D.Decoder CharacterSummaryElement
characterSummaryElementDecoder =
    D.map2 CharacterSummaryElement
        (D.field "scoreSummary" scoreDataDecoder)
        (D.field "scoreRanking" <| D.list playResultDecoder)


type alias CharacterSummary =
    { character : CharacterName
    , normal : Maybe CharacterSummaryElement
    , hard : Maybe CharacterSummaryElement
    }


emptyCharacterSummary =
    { character = "", normal = Nothing, hard = Nothing }


characterSummaryDecoder : D.Decoder CharacterSummary
characterSummaryDecoder =
    D.map3 CharacterSummary
        (D.field "character" D.string)
        (D.maybe <| D.field "normal" characterSummaryElementDecoder)
        (D.maybe <| D.field "hard" characterSummaryElementDecoder)
