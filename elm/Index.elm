module Index exposing (Model, Msg(..), Page(..), PageForParser(..), characterImageUrl, characterNames, characterSummaryReloader, checkbox, checkboxMode, checkboxOrder, checkboxProperty, convPage, enumParser, formatComma, gameModeToSortQueryString, getTimeZone, init, isLoading, loadingIcon, main, parseUrl, reloadButton, sortCharacters, sortCore, sortOrderToQueryString, sortPropertyToQueryString, sortStateQueryParser, sortStateToQueryString, subscriptions, tagHighScore, tagScore, toListH, turnOverOrder, update, view, viewArmament, viewCharacterList, viewCharacterPage, viewCharacterResultCore, viewCharacterScoreRanking, viewDailyPlayResultPage, viewDailyPlayResultPageCore, viewDailySummary, viewDashboardPage, viewHeader, viewMissSituation, viewNotFoundPage, viewPlayResult, viewPlayResultCore, viewPlayResultWithCharacterImage, viewScoreRanking, viewScoreRankingPage, viewScoreSummary, viewScoreSummaryCore, viewTotalResult)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import List.Extra as LE
import List.Split as LS
import Maybe.Extra as ME
import RemoteResource as RR exposing (RemoteResource)
import Set
import Task
import Time exposing (Posix, Zone)
import Times exposing (ZonedTime)
import Types exposing (..)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (..)
import Url.Parser.Query as Query


main : Platform.Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type PageForParser
    = DashboardPageW SortState
    | NotFoundPageW
    | ScoreRankingPageW
    | CharacterPageW CharacterName
    | DailyResultPageW


type Page
    = DashboardPage
    | NotFoundPage
    | ScoreRankingPage
    | CharacterPage CharacterName
    | DailyResultPage


convPage : PageForParser -> Page
convPage src =
    case src of
        NotFoundPageW ->
            NotFoundPage

        DashboardPageW _ ->
            DashboardPage

        ScoreRankingPageW ->
            ScoreRankingPage

        CharacterPageW name ->
            CharacterPage <| Maybe.withDefault name <| Url.percentDecode name

        DailyResultPageW ->
            DailyResultPage


type alias Model =
    { key : Key
    , zone : Zone
    , page : Page
    , characters : RemoteResource CharacterList
    , charactersSortState : SortState
    , totalResult : RemoteResource TotalResult
    , scoreRanking : RemoteResource ModePlayResults
    , characterResultList : Dict CharacterName (RemoteResource CharacterResult)
    , dailyResult : RemoteResource DailyResultWork
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GetTimeZone Zone
    | CharacterListLoaded (Result Http.Error CharacterList)
    | LoadCharacterList
    | CharactersSortStateChanged SortState
    | TotalResultLoaded (Result Http.Error TotalResult)
    | LoadTotalResult
    | ScoreRankingLoaded (Result Http.Error ModePlayResults)
    | LoadScoreRanking
    | CharacterResultLoaded CharacterName (Result Http.Error CharacterResult)
    | LoadCharacterResult CharacterName
    | DailyResultLoaded (Result Http.Error DailyResultWork)
    | LoadDailyResult


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform GetTimeZone Time.here


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            parseUrl url

        model =
            { key = key
            , zone = Time.utc
            , page = convPage page
            , characters = RR.withDummyData <| List.map emptyCharacterScore characterNames
            , charactersSortState = initialSortState
            , totalResult = RR.empty
            , scoreRanking = RR.empty
            , characterResultList = Dict.empty
            , dailyResult = RR.empty
            }
    in
    case page of
        DashboardPageW sortState ->
            ( { model
                | characters = RR.startLoading model.characters
                , charactersSortState = sortState
                , totalResult = RR.startLoading model.totalResult
              }
            , Cmd.batch
                [ Api.getCharacterList CharacterListLoaded
                , Api.getTotalResult TotalResultLoaded
                , getTimeZone
                ]
            )

        NotFoundPageW ->
            ( model, Task.perform GetTimeZone Time.here )

        ScoreRankingPageW ->
            ( { model | scoreRanking = RR.startLoading model.scoreRanking }
            , Cmd.batch
                [ Api.getScoreRanking ScoreRankingLoaded
                , getTimeZone
                ]
            )

        CharacterPageW name ->
            let
                characterName =
                    Maybe.withDefault name <| Url.percentDecode name
            in
            ( { model
                | characterResultList = Dict.insert characterName RR.emptyLoading model.characterResultList
              }
            , Cmd.batch
                [ Api.getCharacterResult characterName <| CharacterResultLoaded characterName
                , getTimeZone
                ]
            )

        DailyResultPageW ->
            ( { model | dailyResult = RR.startLoading model.dailyResult }
            , Cmd.batch
                [ Api.getDailyResult DailyResultLoaded
                , getTimeZone
                ]
            )


parseUrl : Url.Url -> PageForParser
parseUrl url =
    Maybe.withDefault NotFoundPageW <|
        Url.Parser.parse
            (Url.Parser.oneOf
                [ Url.Parser.map DashboardPageW (Url.Parser.top <?> sortStateQueryParser)
                , Url.Parser.map ScoreRankingPageW (Url.Parser.s "score-ranking")
                , Url.Parser.map CharacterPageW (Url.Parser.s "character" </> Url.Parser.string)
                , Url.Parser.map DailyResultPageW (Url.Parser.s "daily-result")
                ]
            )
            url


enumParser : String -> a -> List ( String, a ) -> Query.Parser a
enumParser name default keyValues =
    Query.custom name
        (\ss ->
            case ss of
                [] ->
                    default

                l ->
                    let
                        s =
                            Set.fromList l

                        mKv =
                            LE.find (\( key, _ ) -> Set.member key s) keyValues
                    in
                    Maybe.withDefault default <| Maybe.map (\( _, value ) -> value) mKv
        )


sortStateQueryParser : Query.Parser SortState
sortStateQueryParser =
    Query.map3 SortState
        (enumParser "sort-property" Name [ ( "name", Name ), ( "high-score", HighScore ), ( "average-score", AverageScore ), ( "play-count", PlayCount ) ])
        (enumParser "sort-game-mode" Hard [ ( "hard", Hard ), ( "normal", Normal ) ])
        (enumParser "sort-order" Ascendant [ ( "ascendant", Ascendant ), ( "descendant", Descendant ), ( "asc", Ascendant ), ( "desc", Descendant ) ])


sortStateToQueryString : SortState -> List Url.Builder.QueryParameter
sortStateToQueryString sortState =
    [ sortPropertyToQueryString sortState.property, gameModeToSortQueryString sortState.mode, sortOrderToQueryString sortState.order ]


gameModeToSortQueryString : GameMode -> Url.Builder.QueryParameter
gameModeToSortQueryString v =
    case v of
        Hard ->
            Url.Builder.string "sort-game-mode" "hard"

        Normal ->
            Url.Builder.string "sort-game-mode" "normal"


sortPropertyToQueryString : SortProperty -> Url.Builder.QueryParameter
sortPropertyToQueryString v =
    case v of
        Name ->
            Url.Builder.string "sort-property" "name"

        HighScore ->
            Url.Builder.string "sort-property" "high-score"

        AverageScore ->
            Url.Builder.string "sort-property" "average-score"

        PlayCount ->
            Url.Builder.string "sort-property" "play-count"


sortOrderToQueryString : SortOrder -> Url.Builder.QueryParameter
sortOrderToQueryString v =
    case v of
        Ascendant ->
            Url.Builder.string "sort-order" "ascendant"

        Descendant ->
            Url.Builder.string "sort-order" "descendant"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                page =
                    parseUrl url

                newModel =
                    { model | page = convPage page }
            in
            case newModel.page of
                DashboardPage ->
                    let
                        ( cs, csCmd ) =
                            RR.loadIfNecessary model.characters (Api.getCharacterList CharacterListLoaded)

                        ( tp, tpCmd ) =
                            RR.loadIfNecessary model.totalResult (Api.getTotalResult TotalResultLoaded)
                    in
                    ( { newModel
                        | characters = cs
                        , totalResult = tp
                      }
                    , Cmd.batch [ csCmd, tpCmd ]
                    )

                NotFoundPage ->
                    ( newModel, Cmd.none )

                ScoreRankingPage ->
                    case RR.loadIfNecessary model.scoreRanking (Api.getScoreRanking ScoreRankingLoaded) of
                        ( res, cmd ) ->
                            ( { newModel | scoreRanking = res }, cmd )

                CharacterPage s ->
                    let
                        characterName =
                            Maybe.withDefault s <| Url.percentDecode s
                    in
                    case Dict.get characterName model.characterResultList of
                        Nothing ->
                            ( { newModel
                                | page = CharacterPage characterName
                                , characterResultList = Dict.insert characterName RR.emptyLoading model.characterResultList
                              }
                            , Api.getCharacterResult characterName <| CharacterResultLoaded characterName
                            )

                        Just inList ->
                            ( { newModel | page = CharacterPage characterName }
                            , Cmd.none
                            )

                DailyResultPage ->
                    let
                        ( res, cmd ) =
                            RR.loadIfNecessary newModel.dailyResult (Api.getDailyResult DailyResultLoaded)
                    in
                    ( { newModel | dailyResult = res }, cmd )

        GetTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        CharacterListLoaded res ->
            case res of
                Err _ ->
                    ( { model | characters = RR.updateData model.characters res }
                    , Cmd.none
                    )

                Ok cs ->
                    ( { model | characters = RR.updateSuccessData model.characters <| sortCharacters model.charactersSortState cs }
                    , Cmd.none
                    )

        LoadCharacterList ->
            ( { model | characters = RR.startLoading model.characters }
            , Api.getCharacterList CharacterListLoaded
            )

        CharactersSortStateChanged sortState ->
            let
                cmd =
                    Nav.replaceUrl model.key <| Url.Builder.absolute [] <| sortStateToQueryString sortState
            in
            case model.characters.data of
                Nothing ->
                    ( { model | charactersSortState = sortState }
                    , cmd
                    )

                Just (Err _) ->
                    ( { model | charactersSortState = sortState }, cmd )

                Just (Ok cs) ->
                    ( { model
                        | charactersSortState = sortState
                        , characters = RR.updateSuccessData model.characters <| sortCharacters sortState cs
                      }
                    , cmd
                    )

        TotalResultLoaded res ->
            case res of
                Err _ ->
                    ( { model | totalResult = RR.updateData model.totalResult res }, Cmd.none )

                Ok s ->
                    ( { model | totalResult = RR.updateSuccessData model.totalResult s }, Cmd.none )

        LoadTotalResult ->
            ( { model | totalResult = RR.startLoading model.totalResult }
            , Api.getTotalResult TotalResultLoaded
            )

        ScoreRankingLoaded res ->
            case res of
                Err _ ->
                    ( { model | scoreRanking = RR.updateData model.scoreRanking res }, Cmd.none )

                Ok s ->
                    ( { model | scoreRanking = RR.updateSuccessData model.scoreRanking s }, Cmd.none )

        LoadScoreRanking ->
            ( { model | scoreRanking = RR.startLoading model.scoreRanking }
            , Api.getScoreRanking ScoreRankingLoaded
            )

        CharacterResultLoaded characterName res ->
            case Dict.get characterName model.characterResultList of
                Nothing ->
                    ( { model
                        | characterResultList =
                            Dict.insert characterName (RR.new <| res) model.characterResultList
                      }
                    , Cmd.none
                    )

                Just inList ->
                    ( { model
                        | characterResultList =
                            Dict.insert characterName (RR.updateData inList res) model.characterResultList
                      }
                    , Cmd.none
                    )

        LoadCharacterResult characterName ->
            case Dict.get characterName model.characterResultList of
                Nothing ->
                    ( { model | characterResultList = Dict.insert characterName (RR.startLoading RR.empty) model.characterResultList }
                    , Api.getCharacterResult characterName <| CharacterResultLoaded characterName
                    )

                Just rr ->
                    ( { model | characterResultList = Dict.update characterName (Maybe.map RR.startLoading) model.characterResultList }
                    , Api.getCharacterResult characterName <| CharacterResultLoaded characterName
                    )

        DailyResultLoaded res ->
            case res of
                Err _ ->
                    ( { model | dailyResult = RR.updateData model.dailyResult res }, Cmd.none )

                Ok s ->
                    ( { model | dailyResult = RR.updateSuccessData model.dailyResult s }, Cmd.none )

        LoadDailyResult ->
            ( { model | dailyResult = RR.startLoading model.dailyResult }
            , Api.getDailyResult DailyResultLoaded
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


isLoading : Model -> Bool
isLoading model =
    model.characters.loading
        || model.totalResult.loading
        || (List.any .loading <| Dict.values model.characterResultList)
        || model.scoreRanking.loading
        || model.dailyResult.loading


view : Model -> Document Msg
view model =
    let
        doc =
            case model.page of
                NotFoundPage ->
                    viewNotFoundPage model

                DashboardPage ->
                    viewDashboardPage model

                ScoreRankingPage ->
                    viewScoreRankingPage model

                CharacterPage name ->
                    viewCharacterPage model name <| Dict.get name model.characterResultList

                DailyResultPage ->
                    viewDailyPlayResultPage model

        loading =
            if isLoading model then
                [ span [ class "fas fa-spinner loading loading-icon" ] [] ]

            else
                []
    in
    { title = doc.title ++ " | Jabara's Time Locker play result"
    , body = loading ++ [ viewHeader ] ++ doc.body
    }


viewDashboardPage : Model -> Document Msg
viewDashboardPage model =
    { title = "Dashboard"
    , body =
        [ div [ class "container dashboard" ] <|
            []
                ++ viewTotalResult model.totalResult
                ++ [ hr [] [] ]
                ++ viewCharacterList model.characters model.charactersSortState
        ]
    }


viewDailyPlayResultPage : Model -> Document Msg
viewDailyPlayResultPage model =
    { title = "Daily result"
    , body =
        [ div [ class "container" ] <|
            h2 [] [ text "Daily result", reloadButton model.dailyResult.loading LoadDailyResult ]
                :: viewDailyPlayResultPageCore model
        ]
    }


viewDailyPlayResultPageCore : Model -> List (Html Msg)
viewDailyPlayResultPageCore model =
    let
        dailyResult =
            model.dailyResult
    in
    case dailyResult.data of
        Nothing ->
            [ span [] [ text "Now loading..." ] ]

        Just (Err _) ->
            [ span [] [ text "Fail loading..." ] ]

        Just (Ok res) ->
            List.concat <|
                List.map
                    (\results ->
                        [ viewScoreSummaryCore (Just { zone = model.zone, time = results.day }) results.summary ]
                            ++ List.map (viewPlayResultWithCharacterImage model.zone True) results.detail
                            ++ [ hr [] [] ]
                    )
                <|
                    Types.convertDailyResultWork model.zone res


viewDailySummary : Zone -> ( List Posix, Dict Int ModeSummaryScore ) -> List (Html Msg)
viewDailySummary zone ( dates, summaries ) =
    List.map
        (\day ->
            let
                t =
                    { zone = zone, time = day }
            in
            viewScoreSummaryCore (Just t) <| Maybe.withDefault emptyModeSummaryScore <| Dict.get (Time.posixToMillis day) summaries
        )
        dates


viewCharacterPage : Model -> CharacterName -> Maybe (RemoteResource CharacterResult) -> Document Msg
viewCharacterPage model characterName mResource =
    { title = characterName ++ " Summary"
    , body =
        [ div [ class "container character-summary" ] <|
            [ img [ src <| characterImageUrl characterName 660 460, class "character" ] []
            , h3 [] <| span [ class "character-name" ] [ text characterName ] :: characterSummaryReloader characterName mResource
            ]
                ++ (case mResource of
                        Nothing ->
                            [ span [] [ text "Now loading..." ]
                            , loadingIcon
                            ]

                        Just rr ->
                            viewCharacterResultCore model.zone characterName rr
                   )
        ]
    }


characterSummaryReloader : CharacterName -> Maybe (RemoteResource CharacterResult) -> List (Html Msg)
characterSummaryReloader characterName mResource =
    case mResource of
        Nothing ->
            []

        Just rr ->
            if not (RR.hasData rr) && rr.loading then
                []

            else
                [ reloadButton rr.loading <| LoadCharacterResult characterName ]


viewCharacterResultCore : Zone -> CharacterName -> RemoteResource CharacterResult -> List (Html Msg)
viewCharacterResultCore zone characterName rr =
    case rr.data of
        Nothing ->
            [ span [] [ text "Now loading..." ]
            , loadingIcon
            ]

        Just (Err _) ->
            [ span [] [ text "Fail loading..." ]
            ]

        Just (Ok data) ->
            [ viewScoreSummary data.summary
            , h1 [] [ text "Score ranking" ]
            , viewCharacterScoreRanking zone Hard data.ranking.hard
            , viewCharacterScoreRanking zone Normal data.ranking.normal
            ]


viewCharacterScoreRanking : Zone -> GameMode -> List PlayResult -> Html Msg
viewCharacterScoreRanking zone mode rankings =
    div [ class "score-ranking-container" ] <|
        h3 [] [ text <| Types.gameModeToString mode ]
            :: (if List.length rankings == 0 then
                    [ span [] [ text "(No record)" ] ]

                else
                    List.map (viewPlayResult zone False) rankings
               )


viewPlayResult : Zone -> Bool -> PlayResult -> Html Msg
viewPlayResult zone showMissSituation result =
    div [ class "score-rank-container" ] <| viewPlayResultCore zone showMissSituation result


viewPlayResultWithCharacterImage : Zone -> Bool -> PlayResult -> Html Msg
viewPlayResultWithCharacterImage zone showMissSituation result =
    div [ class "score-rank-container" ] <|
        a [ href <| "/character/" ++ result.character ]
            [ div [ class "character-image-container" ]
                [ img [ src <| characterImageUrl result.character 65 65, alt result.character, class "character" ] []
                , span [ class "character-name" ] [ text result.character ]
                ]
            ]
            :: viewPlayResultCore zone showMissSituation result


viewMissSituation : Bool -> PlayResult -> Maybe (Html Msg)
viewMissSituation showMissSituation result =
    if showMissSituation then
        Just <|
            div [ class "miss-situation" ]
                [ div []
                    [ span [ class "miss-situation-label" ] [ text "Miss situation" ]
                    , span [ class "miss-situation" ] [ text result.missSituation ]
                    ]
                , div []
                    [ span [ class "miss-situation-label" ] [ text "and reason(s)" ]
                    , span [ class "miss-situation" ] [ text <| String.join ", " result.reasons ]
                    ]
                ]

    else
        Nothing


viewPlayResultCore : Zone -> Bool -> PlayResult -> List (Html Msg)
viewPlayResultCore zone showMissSituation result =
    div []
        [ span [ class <| "score-label " ++ (String.toLower <| Types.gameModeToString result.mode) ] [ text "Score: " ]
        , span [ class <| "score " ++ (String.toLower <| Types.gameModeToString result.mode) ] [ text <| formatComma result.score ]
        , span [ class "play-time" ] [ text <| Times.omitSecond zone result.playTime ]
        ]
        :: (toListH <| viewMissSituation showMissSituation result)
        ++ [ div [ class "armaments-container" ] <| List.map viewArmament result.armaments
           ]


viewArmament : Armament -> Html Msg
viewArmament arm =
    div [ class "armament-container" ]
        [ img [ class "armament", src <| "https://static.time-locker.jabara.info/img/ARM_" ++ String.replace " " "_" arm.name ++ ".png", alt arm.name ] []
        , span [ class "armament-level" ] [ text <| formatComma arm.level ]
        ]


viewScoreSummaryCore : Maybe ZonedTime -> ModeSummaryScore -> Html Msg
viewScoreSummaryCore mTime summary =
    let
        mHardScore =
            summary.hard

        mNormalScore =
            summary.normal

        playCountMapper =
            formatComma << .playCount

        highScoreMapper =
            formatComma << .highScore

        averageScoreMapper =
            formatComma << round << .averageScore
    in
    table [ class "score-table score-summary-container" ]
        [ thead []
            [ tr []
                [ th [] <| Maybe.withDefault [] <| Maybe.map (\time -> [ h3 [] [ text <| Times.omitHour2 time ] ]) mTime
                , th [ class "number" ] [ h3 [] [ text "Hard" ] ]
                , th [ class "number" ] [ h3 [] [ text "Normal" ] ]
                ]
            ]
        , tbody []
            [ tr []
                [ th [] [ text "Play count" ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map playCountMapper mHardScore ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map playCountMapper mNormalScore ]
                ]
            , tr []
                [ th [] [ text "High score" ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map highScoreMapper mHardScore ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map highScoreMapper mNormalScore ]
                ]
            , tr []
                [ th [] [ text "Average score" ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map averageScoreMapper mHardScore ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map averageScoreMapper mNormalScore ]
                ]
            ]
        ]


viewScoreSummary : ModeSummaryScore -> Html Msg
viewScoreSummary =
    viewScoreSummaryCore Nothing


viewNotFoundPage : Model -> Document Msg
viewNotFoundPage _ =
    { title = "Not found"
    , body = [ h1 [] [ text "Not Found" ] ]
    }


viewScoreRankingPage : Model -> Document Msg
viewScoreRankingPage model =
    { title = "Score ranking"
    , body =
        [ div [ class "container" ] <|
            viewScoreRanking model.zone model.scoreRanking
        ]
    }


checkbox : String -> Bool -> msg -> Html msg
checkbox labelText checked_ handler =
    label [ onClick handler ]
        [ span [ classList [ ( "fas fa-check", True ), ( "checked", checked_ ), ( "unchecked", not checked_ ) ] ] []
        , span [] [ text labelText ]
        ]


checkboxOrder : String -> SortState -> SortOrder -> Html Msg
checkboxOrder labelText sortState order =
    checkbox labelText (sortState.order == order) <| CharactersSortStateChanged { sortState | order = order }


checkboxMode : String -> SortState -> GameMode -> Html Msg
checkboxMode labelText sortState mode =
    checkbox labelText (sortState.mode == mode) <| CharactersSortStateChanged { sortState | mode = mode }


checkboxProperty : String -> SortState -> SortProperty -> Html Msg
checkboxProperty labelText sortState property =
    checkbox labelText (sortState.property == property) <| CharactersSortStateChanged { sortState | property = property }


viewHeader : Html Msg
viewHeader =
    header []
        [ h1 [] [ a [ href "/" ] [ text "Time Locker result" ] ]
        , nav []
            [ a [ href "/" ] [ text "Dashboard" ]
            , a [ href "/daily-result" ] [ text "Daily result" ]
            , a [ href "/score-ranking" ] [ text "Score ranking" ]
            ]
        ]


viewTotalResult : RemoteResource TotalResult -> List (Html Msg)
viewTotalResult rr =
    let
        fixParts =
            h2 [] [ text "Total", reloadButton rr.loading LoadTotalResult ]
    in
    case rr.data of
        Nothing ->
            [ fixParts, span [] [ text "Now loading..." ] ]

        Just (Err _) ->
            [ fixParts, span [] [ text "Fail loading characters..." ] ]

        Just (Ok totalResult) ->
            [ fixParts
            , viewScoreSummary { hard = Just totalResult.hard, normal = Just totalResult.normal }
            ]


viewScoreRanking : Zone -> RemoteResource ModePlayResults -> List (Html Msg)
viewScoreRanking zone rr =
    let
        fixParts =
            h2 [] [ text "Score ranking", reloadButton rr.loading LoadScoreRanking ]
    in
    case rr.data of
        Nothing ->
            [ fixParts, span [] [ text "Now loading..." ] ]

        Just (Err _) ->
            [ fixParts, span [] [ text "Fail loading characters..." ] ]

        Just (Ok scoreRanking) ->
            [ fixParts
            , div [ class "score-ranking-container" ] <|
                h3 [] [ text "Hard" ]
                    :: (List.map (viewPlayResultWithCharacterImage zone False) <| List.take 10 scoreRanking.hard)
            , div [ class "score-ranking-container" ] <|
                h3 [] [ text "Normal" ]
                    :: (List.map (viewPlayResultWithCharacterImage zone False) <| List.take 10 scoreRanking.normal)
            ]


viewCharacterList : RemoteResource CharacterList -> SortState -> List (Html Msg)
viewCharacterList characters sortState =
    let
        fixParts =
            h2 [] [ text "Character list", reloadButton characters.loading LoadCharacterList ]
    in
    case characters.data of
        Nothing ->
            [ fixParts, span [] [ text "Now loading..." ] ]

        Just (Err _) ->
            [ fixParts, span [] [ text "Fail loading characters..." ] ]

        Just (Ok cs) ->
            fixParts
                :: div []
                    [ span [ class "character-count" ] [ text <| String.fromInt <| List.length cs ]
                    , span [] [ text " characters." ]
                    ]
                :: div [ class "sort-controller-container" ]
                    [ h4 [] [ text "Sort" ]
                    , div [ class "sort-parameter" ]
                        [ checkboxProperty "Name" sortState Name
                        , checkboxProperty "High score" sortState HighScore
                        , checkboxProperty "Average score" sortState AverageScore
                        , checkboxProperty "Play count" sortState PlayCount
                        ]
                    , div [ class "sort-parameter" ]
                        [ checkboxOrder "Ascendant" sortState Ascendant
                        , checkboxOrder "Descendant" sortState Descendant
                        ]
                    , div [ class "sort-parameter" ]
                        [ checkboxMode "Hard" sortState Hard
                        , checkboxMode "Normal" sortState Normal
                        ]
                    ]
                :: List.map
                    (\character ->
                        a [ href <| "/character/" ++ character.character ]
                            [ div [ class "character-image-container-floating" ]
                                [ img [ src <| characterImageUrl character.character 65 65, alt character.character, class "character" ] []
                                , span [ class "character-name" ] [ text character.character ]
                                , tagScore character
                                ]
                            ]
                    )
                    cs


tagScore : CharacterScore -> Html.Html msg
tagScore c =
    table [ class "score-table character-list" ]
        [ tbody [] <|
            [ tr [] [ th [] [ text "Hard" ] ] ]
                ++ tagHighScore c.hard
                ++ [ tr [] [ th [] [ text "Normal" ] ] ]
                ++ tagHighScore c.normal
        ]


tagHighScore : Maybe SummaryScore -> List (Html.Html msg)
tagHighScore mScore =
    [ tr [ class "score-element" ]
        [ th [] [ text "Play count" ]
        , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map (\score -> formatComma score.playCount) mScore ]
        ]
    , tr [ class "score-element" ]
        [ th [] [ text "High score" ]
        , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map (\score -> formatComma score.highScore) mScore ]
        ]
    , tr [ class "score-element" ]
        [ th [] [ text "Average score" ]
        , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map (\score -> formatComma <| round score.averageScore) mScore ]
        ]
    ]


characterImageUrl : CharacterName -> Int -> Int -> String
characterImageUrl characterName width height =
    "https://static.time-locker.jabara.info/img/" ++ characterName ++ "@" ++ String.fromInt width ++ "x" ++ String.fromInt height ++ ".png"


formatComma : Int -> String
formatComma =
    String.join "," << List.reverse << List.map String.fromList << LS.chunksOfRight 3 << String.toList << String.fromInt


loadingIcon : Html.Html msg
loadingIcon =
    span [ class "fas fa-sync loading" ] []


reloadButton : Bool -> Msg -> Html Msg
reloadButton loading handler =
    button
        [ class "btn btn-default reloader"
        , onClick handler
        ]
        [ i [ classList [ ( "fas fa-sync", True ), ( "loading", loading ) ] ] [] ]


sortCore : SortOrder -> (CharacterScore -> number) -> CharacterList -> CharacterList
sortCore order comparator =
    case order of
        Ascendant ->
            List.sortBy comparator

        Descendant ->
            List.sortBy (negate << comparator)


sortCharacters : SortState -> CharacterList -> CharacterList
sortCharacters sortState origin =
    case sortState.property of
        HighScore ->
            let
                f =
                    if sortState.mode == Hard then
                        .hard

                    else
                        .normal
            in
            sortCore sortState.order (Maybe.withDefault 0 << Maybe.map .highScore << f) origin

        AverageScore ->
            let
                f =
                    if sortState.mode == Hard then
                        .hard

                    else
                        .normal
            in
            sortCore sortState.order (Maybe.withDefault 0 << Maybe.map .averageScore << f) origin

        PlayCount ->
            let
                f =
                    if sortState.mode == Hard then
                        .hard

                    else
                        .normal
            in
            sortCore sortState.order (Maybe.withDefault 0 << Maybe.map .playCount << f) origin

        Name ->
            let
                f =
                    if sortState.mode == Hard then
                        .hard

                    else
                        .normal
            in
            List.sortWith (\c0 c1 -> turnOverOrder sortState.order <| compare c0.character c1.character) origin


turnOverOrder : SortOrder -> Order -> Order
turnOverOrder order src =
    case ( order, src ) of
        ( _, EQ ) ->
            EQ

        ( Ascendant, o ) ->
            o

        ( Descendant, GT ) ->
            LT

        ( Descendant, LT ) ->
            GT


toListH : Maybe (Html msg) -> List (Html msg)
toListH =
    Maybe.withDefault [] << Maybe.map (\e -> [ e ])


characterNames =
    [ "ACUTE WIDE LOCKER"
    , "ALLIGATOR"
    , "ALLO SAURUS"
    , "ANT BEAR"
    , "APPLIV WALKER"
    , "AUTO AIM BOT"
    , "AUTO ICE LASER"
    , "BACK SHOOTER"
    , "BACK SPRAY SHOOTER"
    , "BEAM DRAGON"
    , "BEAM PSYCHIC"
    , "BEAM WALKER"
    , "BIG LINE LOCKER"
    , "BIO RIDER"
    , "D RIFLE LOCKER"
    , "DIFFUSER"
    , "DIMETRODON"
    , "DOUBLE SNIPER"
    , "FARTER"
    , "FLAP SNIPER"
    , "FLAT LOCKER"
    , "FREEZER"
    , "GAME CAST"
    , "GORI WRAP"
    , "GREEN MARKER"
    , "HOMING HOPPER"
    , "HOMING ICE BOT"
    , "HUMMER HEAD"
    , "HUNTER KILLER"
    , "HUSKY"
    , "ICE BEAM LOCKER"
    , "ICE LINE LOCKER"
    , "ICE PTERANODON"
    , "JUSTIN"
    , "LAUNCHER"
    , "LAUNCHER 2"
    , "MAD LOCKER"
    , "MINE DRIVER"
    , "MINE LOCKER"
    , "MINIGUN LOCKER"
    , "MISSILE MASTER"
    , "MISSILE MASTER 2"
    , "MUCUS"
    , "PANDA"
    , "PEE RASCAL"
    , "PENGUIN"
    , "PLESIO SAUR"
    , "PREDATOR"
    , "PSYCHIC LOCKER"
    , "PTERANODON"
    , "QUAD LOCKER"
    , "RIFLE LOCKER"
    , "ROCKET LOCKER"
    , "RODEO STAMPEDE I"
    , "RODEO STAMPEDE Ⅱ"
    , "SHIKIISHI LOCKER"
    , "SIDE LOCKER"
    , "SKATER"
    , "SPEED-MSL DOG"
    , "SPEED-RCT DIATRYMA"
    , "SPRAY WALKER"
    , "STEGO SAUR"
    , "SUPPORTER BEAR"
    , "T-REX"
    , "THE DOG"
    , "THE LOCKER"
    , "TORTOISE"
    , "TRACKER"
    , "TRIKE"
    , "TWINKIE DRONE"
    , "WAR DRONE"
    , "WAR FROG"
    , "WAR MANMOTH"
    , "WAR TOY"
    , "WHALE"
    , "WIDE BREAKER"
    , "WIDE ICE LOCKER"
    , "WIDE JUSTIN"
    , "WIDE RHINO"
    , "X-LASER"
    , "X-SHOOTER"
    ]
