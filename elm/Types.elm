module Types exposing (..)

import Json.Decode as D
import Maybe.Extra as ME
import Time
import Times


type alias CharacterName =
    String


type alias CharacterScore =
    { character : CharacterName
    , normal : Maybe SummaryScore
    , hard : Maybe SummaryScore
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


getScoreForMode : GameMode -> CharacterScore -> Maybe SummaryScore
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
        (D.maybe <| D.field "normal" summaryScoreDecoder)
        (D.maybe <| D.field "hard" summaryScoreDecoder)


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


type alias SummaryScore =
    { playCount : Int
    , highScore : Int
    , averageScore : Float
    }


emptySummaryScore =
    { playCount = 0
    , highScore = 0
    , averageScore = 0
    }


summaryScoreDecoder : D.Decoder SummaryScore
summaryScoreDecoder =
    D.map3 SummaryScore
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)


type alias ModeSummaryScore =
    { hard : Maybe SummaryScore
    , normal : Maybe SummaryScore
    }


emptyModeSummaryScore =
    { hard = emptySummaryScore
    , normal = emptySummaryScore
    }


modeSummaryScoreDecoder : D.Decoder ModeSummaryScore
modeSummaryScoreDecoder =
    D.map2 ModeSummaryScore
        (D.maybe <| D.field "hard" summaryScoreDecoder)
        (D.maybe <| D.field "normal" summaryScoreDecoder)


type alias PlayResult =
    { character : CharacterName
    , mode : GameMode
    , score : Int
    , armaments : List Armament
    , reasons : List String
    , playTime : Time.Posix
    , missSituation : String
    }


playResultDecoder : D.Decoder PlayResult
playResultDecoder =
    D.map7 PlayResult
        (D.field "character" D.string)
        (D.field "mode" gameModeDecoder)
        (D.field "score" <| nvlDecoder 0 D.int)
        (D.field "armaments" <| D.list armamentDecoder)
        (D.field "reasons" <| D.list D.string)
        (D.field "created" D.string |> D.andThen (\s -> D.succeed <| Times.parseDatetime s))
        (D.field "missSituation" D.string)


type alias ModePlayResults =
    { hard : List PlayResult
    , normal : List PlayResult
    }


emptyModePlayResults =
    { hard = [], normal = [] }


flattenPlayResults : ModePlayResults -> List PlayResult
flattenPlayResults prs =
    prs.hard ++ prs.normal |> List.sortWith (\r0 r1 -> compare (Time.posixToMillis r1.playTime) (Time.posixToMillis r0.playTime))


modePlayResultsDecoder : D.Decoder ModePlayResults
modePlayResultsDecoder =
    D.map2 ModePlayResults
        (D.field "hard" <| D.list playResultDecoder)
        (D.field "normal" <| D.list playResultDecoder)


type alias CharacterResult =
    { character : CharacterName
    , summary : ModeSummaryScore
    , ranking : ModePlayResults
    , detail : ModePlayResults
    }


emptyCharacterResult =
    { character = "", summary = emptyModeSummaryScore, ranking = emptyModePlayResults, detail = emptyModePlayResults }


characterResultDecoder : D.Decoder CharacterResult
characterResultDecoder =
    D.map4 CharacterResult
        (D.field "character" D.string)
        (D.field "summary" modeSummaryScoreDecoder)
        (D.field "ranking" modePlayResultsDecoder)
        (D.field "detail" modePlayResultsDecoder)


type alias TotalResult =
    { hard : SummaryScore
    , normal : SummaryScore
    }


totalResultDecoder : D.Decoder TotalResult
totalResultDecoder =
    D.map2 TotalResult
        (D.field "hard" summaryScoreDecoder)
        (D.field "normal" summaryScoreDecoder)


type alias DailyScore =
    { playCount : Int
    , highScore : Int
    , averageScore : Float
    , playDate : Time.Posix
    }


dailyScoreDecoder : D.Decoder DailyScore
dailyScoreDecoder =
    D.map4 DailyScore
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)
        (D.field "playDate" D.string |> D.andThen (\s -> D.succeed <| Times.parseDatetime <| s ++ "T00:00:00.000Z"))


type alias ModeDailyScores =
    { hard : List DailyScore
    , normal : List DailyScore
    }


modeDailyScoresDecoder : D.Decoder ModeDailyScores
modeDailyScoresDecoder =
    D.map2 ModeDailyScores
        (D.field "hard" <| D.list dailyScoreDecoder)
        (D.field "normal" <| D.list dailyScoreDecoder)


type alias DailyResult =
    { summary : ModeDailyScores
    , detail : ModePlayResults
    }


dailyResultDecoder : D.Decoder DailyResult
dailyResultDecoder =
    D.map2 DailyResult
        (D.field "summary" modeDailyScoresDecoder)
        (D.field "detail" modePlayResultsDecoder)


nvlDecoder : a -> D.Decoder a -> D.Decoder a
nvlDecoder defaultValue decoder =
    D.andThen (ME.unwrap (D.succeed defaultValue) D.succeed) <| D.maybe decoder
