module Types exposing (Armament, CharacterList, CharacterName, CharacterResult, CharacterScore, DailyResult, DailyResultList, DailyResultWork, DailySummaryScore, GameMode(..), ModeDailyScores, ModePlayResults, ModeSummaryScore, PlayResult, SortOrder(..), SortProperty(..), SortState, SummaryScore, TotalResult, armamentDecoder, characterListDecoder, characterListElementDecoder, characterResultDecoder, convertDailyResultWork, dailyResultWorkDecoder, dailySummaryScoreDecoder, dailySummaryToSummary, emptyCharacterResult, emptyCharacterScore, emptyModePlayResults, emptyModeSummaryScore, emptySummaryScore, fillDates, flatten, gameModeDecoder, gameModeToString, getScoreForMode, initialSortState, modeDailyScoresDecoder, modePlayResultsDecoder, modeSummaryScoreDecoder, nvlDecoder, playResultDecoder, summaryScoreDecoder, totalResultDecoder)

import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode as D
import Maybe.Extra as ME
import Time exposing (Posix, Zone)
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
    , playDate : Date
    }


dailySummaryScoreDecoder : D.Decoder DailySummaryScore
dailySummaryScoreDecoder =
    D.map4 DailySummaryScore
        (D.field "playCount" D.int)
        (D.field "highScore" D.int)
        (D.field "averageScore" D.float)
        (D.field "playDate" D.string |> D.andThen (D.succeed << Result.withDefault defaultDate << Date.fromIsoString))


defaultPosix : Posix
defaultPosix =
    Time.millisToPosix 0


defaultDate : Date
defaultDate =
    Date.fromPosix Time.utc defaultPosix


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
    { day : Date
    , summary : ModeSummaryScore
    , detail : List PlayResult
    }


type alias DailyResultList =
    List DailyResult


convertDailyResultWork : Zone -> DailyResultWork -> DailyResultList
convertDailyResultWork zone work =
    let
        milliList =
            List.map Date.toRataDie <| List.map .playDate <| work.summary.normal ++ work.summary.hard
    in
    case ( List.minimum milliList, List.maximum milliList ) of
        ( Nothing, _ ) ->
            []

        ( _, Nothing ) ->
            []

        ( Just minMilli, Just maxMilli ) ->
            let
                dates =
                    fillDates (Date.fromRataDie maxMilli) [] (Date.fromRataDie minMilli)

                summariesHard =
                    Dict.fromList <| List.map (\s -> ( Date.toRataDie s.playDate, dailySummaryToSummary s )) work.summary.hard

                summariesNormal =
                    Dict.fromList <| List.map (\s -> ( Date.toRataDie s.playDate, dailySummaryToSummary s )) work.summary.normal

                details =
                    flatten zone work.detail
            in
            List.map
                (\day ->
                    let
                        key =
                            Date.toRataDie day
                    in
                    case ( Dict.get key summariesHard, Dict.get key summariesNormal, Dict.get key details ) of
                        ( Nothing, Nothing, Nothing ) ->
                            { day = day, summary = emptyModeSummaryScore, detail = [] }

                        ( Just summaryHard, Nothing, Nothing ) ->
                            { day = day
                            , summary = { hard = Just summaryHard, normal = Nothing }
                            , detail = []
                            }

                        ( Nothing, Just summaryNormal, Nothing ) ->
                            { day = day
                            , summary = { hard = Nothing, normal = Just summaryNormal }
                            , detail = []
                            }

                        ( Nothing, Nothing, Just detail ) ->
                            { day = day
                            , summary = emptyModeSummaryScore
                            , detail = detail
                            }

                        ( Just summaryHard, Just summaryNormal, Nothing ) ->
                            { day = day
                            , summary = { hard = Just summaryHard, normal = Just summaryNormal }
                            , detail = []
                            }

                        ( Just summaryHard, Nothing, Just detail ) ->
                            { day = day
                            , summary = { hard = Just summaryHard, normal = Nothing }
                            , detail = detail
                            }

                        ( Nothing, Just summaryNormal, Just detail ) ->
                            { day = day
                            , summary = { hard = Nothing, normal = Just summaryNormal }
                            , detail = detail
                            }

                        ( Just summaryHard, Just summaryNormal, Just detail ) ->
                            { day = day
                            , summary = { hard = Just summaryHard, normal = Just summaryNormal }
                            , detail = detail
                            }
                )
                dates


flatten : Zone -> ModePlayResults -> Dict Int (List PlayResult)
flatten zone results =
    List.foldr
        (\result accumlator ->
            let
                key =
                    Date.toRataDie <| Date.fromPosix zone result.playTime
            in
            if Dict.member key accumlator then
                Dict.update key (Maybe.map (\rs -> [ result ] ++ rs)) accumlator

            else
                Dict.insert key [ result ] accumlator
        )
        Dict.empty
    <|
        results.hard
            ++ results.normal


type alias DailyResultWork =
    { summary : ModeDailyScores
    , detail : ModePlayResults
    }


dailyResultWorkDecoder : D.Decoder DailyResultWork
dailyResultWorkDecoder =
    D.map2 DailyResultWork
        (D.field "summary" modeDailyScoresDecoder)
        (D.field "detail" modePlayResultsDecoder)


fillDates : Date -> List Date -> Date -> List Date
fillDates max accumlator current =
    let
        maxMillis =
            Date.toRataDie max

        curMillis =
            Date.toRataDie current
    in
    if maxMillis == curMillis then
        current :: accumlator

    else
        fillDates max (current :: accumlator) (Date.add Date.Days 1 current)


dailySummaryToSummary : DailySummaryScore -> SummaryScore
dailySummaryToSummary src =
    { playCount = src.playCount
    , highScore = src.highScore
    , averageScore = src.averageScore
    }


nvlDecoder : a -> D.Decoder a -> D.Decoder a
nvlDecoder defaultValue decoder =
    D.andThen (ME.unwrap (D.succeed defaultValue) D.succeed) <| D.maybe decoder
