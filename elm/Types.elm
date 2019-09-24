module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Maybe.Extra as ME
import Time exposing (Posix)
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


type alias DailySummaryScore =
    { playCount : Int
    , highScore : Int
    , averageScore : Float
    , playDate : Posix
    }


dailySummaryScoreDecoder : D.Decoder DailySummaryScore
dailySummaryScoreDecoder =
    D.map4 DailySummaryScore
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)
        (D.field "playDate" D.string |> D.andThen (\s -> D.succeed <| Times.parseDatetime <| s ++ "T00:00:00.000Z"))


type alias ModeSummaryScore =
    { hard : Maybe SummaryScore
    , normal : Maybe SummaryScore
    }


emptyModeSummaryScore =
    { hard = Nothing
    , normal = Nothing
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
    , playTime : Posix
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


type alias ModeDailyScores =
    { hard : List DailySummaryScore
    , normal : List DailySummaryScore
    }


modeDailyScoresDecoder : D.Decoder ModeDailyScores
modeDailyScoresDecoder =
    D.map2 ModeDailyScores
        (D.field "hard" <| D.list dailySummaryScoreDecoder)
        (D.field "normal" <| D.list dailySummaryScoreDecoder)


type alias DailyResult =
    { summary : ( List Posix, Dict Int ModeSummaryScore )
    , detail : ModePlayResults
    }


dailyResultDecoder : D.Decoder DailyResult
dailyResultDecoder =
    D.map2 DailyResult
        (D.field "summary" modeDailyScoresDecoder |> D.andThen (D.succeed << modeDailySummaryToDailySummary))
        (D.field "detail" modePlayResultsDecoder)


modeDailySummaryToDailySummary : ModeDailyScores -> ( List Posix, Dict Int ModeSummaryScore )
modeDailySummaryToDailySummary src =
    let
        milliList =
            List.map Time.posixToMillis <| List.map .playDate <| src.hard ++ src.normal
    in
    case ( List.minimum milliList, List.maximum milliList ) of
        ( Nothing, _ ) ->
            ( [], Dict.empty )

        ( _, Nothing ) ->
            ( [], Dict.empty )

        ( Just minMilli, Just maxMilli ) ->
            ( fillDates (Time.millisToPosix maxMilli) [] (Time.millisToPosix minMilli)
            , dailyToMode src
            )


dailyToMode : ModeDailyScores -> Dict Int ModeSummaryScore
dailyToMode scores =
    Dict.merge
        (\k v -> Dict.insert k v)
        (\k h n -> Dict.update k (\_ -> Just { hard = h.hard, normal = n.normal }))
        (\k v -> Dict.insert k v)
        (Dict.fromList <| List.map (\score -> ( Time.posixToMillis score.playDate, dailySummaryScoreToModeSummaryScore Hard score )) scores.hard)
        (Dict.fromList <| List.map (\score -> ( Time.posixToMillis score.playDate, dailySummaryScoreToModeSummaryScore Normal score )) scores.normal)
        Dict.empty


fillDates : Posix -> List Posix -> Posix -> List Posix
fillDates max accumlator current =
    let
        maxMillis =
            Time.posixToMillis max

        curMillis =
            Time.posixToMillis current
    in
    if maxMillis == curMillis then
        accumlator

    else
        fillDates max (current :: accumlator) (Time.millisToPosix <| curMillis + (1000 * 60 * 60 * 24))


dailySummaryScoreToModeSummaryScore : GameMode -> DailySummaryScore -> ModeSummaryScore
dailySummaryScoreToModeSummaryScore mode src =
    case mode of
        Hard ->
            { normal = Nothing
            , hard =
                Just
                    { highScore = src.highScore
                    , averageScore = src.averageScore
                    , playCount = src.playCount
                    }
            }

        Normal ->
            { hard = Nothing
            , normal =
                Just
                    { highScore = src.highScore
                    , averageScore = src.averageScore
                    , playCount = src.playCount
                    }
            }


nvlDecoder : a -> D.Decoder a -> D.Decoder a
nvlDecoder defaultValue decoder =
    D.andThen (ME.unwrap (D.succeed defaultValue) D.succeed) <| D.maybe decoder
