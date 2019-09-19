module Types exposing (..)

import Json.Decode as D
import Maybe.Extra as ME
import Time
import Times


type alias CharacterName =
    String


type alias CharacterScore =
    { character : CharacterName
    , normal : Maybe ScoreData
    , hard : Maybe ScoreData
    }


emptyCharacterScore : CharacterName -> CharacterScore
emptyCharacterScore cn =
    { character = cn, hard = Nothing, normal = Nothing }


type GameMode
    = Hard
    | Normal


gameModeToString : GameMode -> String
gameModeToString mode =
    case mode of
        Hard ->
            "Hard"

        Normal ->
            "Normal"


gameModeDecoder : D.Decoder GameMode
gameModeDecoder =
    D.andThen
        (\s ->
            case String.toUpper s of
                "HARD" ->
                    D.succeed Hard

                _ ->
                    D.succeed Normal
        )
        D.string


type SortProperty
    = HighScore
    | AverageScore
    | Name
    | PlayCount


type SortOrder
    = Ascendant
    | Descendant


type alias SortState =
    { property : SortProperty
    , mode : GameMode
    , order : SortOrder
    }


initialSortState =
    { property = Name
    , mode = Hard
    , order = Ascendant
    }


getScoreForMode : GameMode -> CharacterScore -> Maybe ScoreData
getScoreForMode mode =
    case mode of
        Hard ->
            .hard

        Normal ->
            .normal


characterListElementDecoder : D.Decoder CharacterScore
characterListElementDecoder =
    D.map3 CharacterScore
        (D.field "character" D.string)
        (D.maybe <| D.field "normal" scoreDataDecoder)
        (D.maybe <| D.field "hard" scoreDataDecoder)


type alias CharacterList =
    List CharacterScore


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
        (D.field "level" <| nvlDecoder 0 D.int)


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
    { character : CharacterName
    , mode : GameMode
    , score : Int
    , armaments : List Armament
    , reasons : List String
    , playTime : Time.Posix
    }


playResultDecoder : D.Decoder PlayResult
playResultDecoder =
    D.map6 PlayResult
        (D.field "character" D.string)
        (D.field "mode" gameModeDecoder)
        (D.field "score" <| nvlDecoder 0 D.int)
        (D.field "armaments" <| D.list armamentDecoder)
        (D.field "reasons" <| D.list D.string)
        (D.field "created" D.string |> D.andThen (\s -> D.succeed <| Times.parseDatetime s))


type alias CharacterSummaryElement =
    { scoreSummary : ScoreData
    , scoreRanking : List PlayResult
    }


characterSummaryElementDecoder : D.Decoder CharacterSummaryElement
characterSummaryElementDecoder =
    D.map2 CharacterSummaryElement
        (D.field "scoreSummary" scoreDataDecoder)
        (D.field "scoreRanking" <| D.list playResultDecoder)


type alias PlayResults =
    { hard : List PlayResult
    , normal : List PlayResult
    }


playResultsDecoder : D.Decoder PlayResults
playResultsDecoder =
    D.map2 PlayResults
        (D.field "hard" <| D.list playResultDecoder)
        (D.field "normal" <| D.list playResultDecoder)


type alias CharacterSummary =
    { character : CharacterName
    , hard : Maybe CharacterSummaryElement
    , normal : Maybe CharacterSummaryElement
    }


emptyCharacterSummary =
    { character = "", normal = Nothing, hard = Nothing }


characterSummaryDecoder : D.Decoder CharacterSummary
characterSummaryDecoder =
    D.map3 CharacterSummary
        (D.field "character" D.string)
        (D.maybe <| D.field "hard" characterSummaryElementDecoder)
        (D.maybe <| D.field "normal" characterSummaryElementDecoder)


type alias TotalPlayState =
    { hard : ScoreData
    , normal : ScoreData
    }


totalPlayStateDecoder : D.Decoder TotalPlayState
totalPlayStateDecoder =
    D.map2 TotalPlayState
        (D.field "hard" scoreDataDecoder)
        (D.field "normal" scoreDataDecoder)


type alias DailyScoreData =
    { playCount : Int
    , highScore : Int
    , averageScore : Float
    , playDate : Time.Posix
    }


dailyScoreDataDecoder : D.Decoder DailyScoreData
dailyScoreDataDecoder =
    D.map4 DailyScoreData
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)
        (D.field "playDate" D.string |> D.andThen (\s -> D.succeed <| Times.parseDatetime <| s ++ "T00:00:00.000Z"))


type alias DailyScoreDatas =
    { hard : List DailyScoreData
    , normal : List DailyScoreData
    }


dailyScoreDatasDecoder : D.Decoder DailyScoreDatas
dailyScoreDatasDecoder =
    D.map2 DailyScoreDatas
        (D.field "hard" <| D.list dailyScoreDataDecoder)
        (D.field "normal" <| D.list dailyScoreDataDecoder)


type alias DailyPlayResult =
    { summary : DailyScoreDatas
    , detail : PlayResults
    }


dailyPlayResultDecoder : D.Decoder DailyPlayResult
dailyPlayResultDecoder =
    D.map2 DailyPlayResult
        (D.field "summary" dailyScoreDatasDecoder)
        (D.field "detail" playResultsDecoder)


nvlDecoder : a -> D.Decoder a -> D.Decoder a
nvlDecoder defaultValue decoder =
    D.andThen (ME.unwrap (D.succeed defaultValue) D.succeed) <| D.maybe decoder
