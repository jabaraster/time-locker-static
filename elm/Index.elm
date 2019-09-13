module Index exposing (..)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
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
import Time
import Types exposing (..)
import Url
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
    | CharacterSummaryPageW CharacterName


type Page
    = DashboardPage
    | NotFoundPage
    | CharacterSummaryPage CharacterName


convPage : PageForParser -> Page
convPage src =
    case src of
        NotFoundPageW ->
            NotFoundPage

        DashboardPageW _ ->
            DashboardPage

        CharacterSummaryPageW name ->
            CharacterSummaryPage <| Maybe.withDefault name <| Url.percentDecode name


type alias Model =
    { key : Nav.Key
    , page : Page
    , characters : RemoteResource CharacterList
    , charactersSortState : SortState
    , totalPlayState : RemoteResource TotalPlayState
    , scoreRanking : RemoteResource ScoreRanking
    , characterSummaryList : Dict CharacterName (RemoteResource CharacterSummary)
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CharacterListLoaded (Result Http.Error CharacterList)
    | LoadCharacterList
    | CharactersSortStateChanged SortState
    | TotalPlayStateLoaded (Result Http.Error TotalPlayState)
    | LoadTotalPlayState
    | ScoreRankingLoaded (Result Http.Error ScoreRanking)
    | LoadScoreRanking
    | CharacterSummaryLoaded CharacterName (Result Http.Error CharacterSummary)
    | LoadCharacterSummary CharacterName


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            parseUrl url

        model =
            { key = key
            , page = convPage page
            , characters = RR.withDummyData <| List.map emptyCharacterScore characterNames
            , charactersSortState = initialSortState
            , totalPlayState = RR.empty
            , scoreRanking = RR.empty
            , characterSummaryList = Dict.empty
            }
    in
    case page of
        DashboardPageW sortState ->
            ( { model
                | characters = RR.startLoading model.characters
                , charactersSortState = sortState
                , totalPlayState = RR.startLoading model.totalPlayState
                , scoreRanking = RR.startLoading model.scoreRanking
              }
            , Cmd.batch
                [ Api.getCharacterList CharacterListLoaded
                , Api.getTotalPlayState TotalPlayStateLoaded
                , Api.getScoreRanking ScoreRankingLoaded
                ]
            )

        NotFoundPageW ->
            ( model, Cmd.none )

        CharacterSummaryPageW name ->
            let
                characterName =
                    Maybe.withDefault name <| Url.percentDecode name
            in
            ( { model
                | characterSummaryList = Dict.insert characterName RR.emptyLoading model.characterSummaryList
              }
            , Api.getCharacterSummary characterName <| CharacterSummaryLoaded characterName
            )


parseUrl : Url.Url -> PageForParser
parseUrl url =
    Maybe.withDefault NotFoundPageW <|
        Url.Parser.parse
            (Url.Parser.oneOf
                [ Url.Parser.map DashboardPageW (Url.Parser.top <?> sortStateQueryParser)
                , Url.Parser.map CharacterSummaryPageW (Url.Parser.s "character" </> Url.Parser.string)
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
                        ( cs, mCsCmd ) =
                            RR.loadIfNecessary model.characters (Api.getCharacterList CharacterListLoaded)

                        ( tp, mTpCmd ) =
                            RR.loadIfNecessary model.totalPlayState (Api.getTotalPlayState TotalPlayStateLoaded)

                        ( sr, mSrCmd ) =
                            RR.loadIfNecessary model.scoreRanking (Api.getScoreRanking ScoreRankingLoaded)
                    in
                    ( { newModel
                        | characters = cs
                        , totalPlayState = tp
                        , scoreRanking = sr
                      }
                    , Cmd.batch <| ME.values [ mCsCmd, mTpCmd, mSrCmd ]
                    )

                NotFoundPage ->
                    ( newModel, Cmd.none )

                CharacterSummaryPage s ->
                    let
                        characterName =
                            Maybe.withDefault s <| Url.percentDecode s
                    in
                    case Dict.get characterName model.characterSummaryList of
                        Nothing ->
                            ( { newModel
                                | page = CharacterSummaryPage characterName
                                , characterSummaryList = Dict.insert characterName RR.emptyLoading model.characterSummaryList
                              }
                            , Api.getCharacterSummary characterName <| CharacterSummaryLoaded characterName
                            )

                        Just inList ->
                            ( { newModel | page = CharacterSummaryPage characterName }
                            , Cmd.none
                            )

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

        TotalPlayStateLoaded res ->
            case res of
                Err _ ->
                    ( { model | totalPlayState = RR.updateData model.totalPlayState res }, Cmd.none )

                Ok s ->
                    ( { model | totalPlayState = RR.updateSuccessData model.totalPlayState s }, Cmd.none )

        LoadTotalPlayState ->
            ( { model | totalPlayState = RR.startLoading model.totalPlayState }
            , Api.getTotalPlayState TotalPlayStateLoaded
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

        CharacterSummaryLoaded characterName res ->
            case Dict.get characterName model.characterSummaryList of
                Nothing ->
                    ( { model
                        | characterSummaryList =
                            Dict.insert characterName (RR.new <| res) model.characterSummaryList
                      }
                    , Cmd.none
                    )

                Just inList ->
                    ( { model
                        | characterSummaryList =
                            Dict.insert characterName (RR.updateData inList res) model.characterSummaryList
                      }
                    , Cmd.none
                    )

        LoadCharacterSummary characterName ->
            case Dict.get characterName model.characterSummaryList of
                Nothing ->
                    ( { model | characterSummaryList = Dict.insert characterName (RR.startLoading RR.empty) model.characterSummaryList }
                    , Api.getCharacterSummary characterName <| CharacterSummaryLoaded characterName
                    )

                Just rr ->
                    ( { model | characterSummaryList = Dict.update characterName (Maybe.map RR.startLoading) model.characterSummaryList }
                    , Api.getCharacterSummary characterName <| CharacterSummaryLoaded characterName
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


isLoading : Model -> Bool
isLoading model =
    model.characters.loading
        || (List.any .loading <| Dict.values model.characterSummaryList)


view : Model -> Document Msg
view model =
    let
        doc =
            case model.page of
                DashboardPage ->
                    viewDashboard model

                NotFoundPage ->
                    viewNotFound model

                CharacterSummaryPage name ->
                    viewCharacterSummary model name <| Dict.get name model.characterSummaryList

        loading =
            if isLoading model then
                [ span [ class "fas fa-spinner loading loading-icon" ] [] ]

            else
                []
    in
    { title = doc.title ++ " | Jabara's Time Locker play result"
    , body = loading ++ doc.body
    }


viewDashboard : Model -> Document Msg
viewDashboard model =
    { title = "Dashboard"
    , body =
        [ viewHeader
        , div [ class "container dashboard" ] <|
            []
                ++ viewTotalPlayState model.totalPlayState
                ++ [ hr [] [] ]
                ++ viewScoreRanking model.scoreRanking
                ++ [ hr [] [] ]
                ++ viewCharacterList model.characters model.charactersSortState
        ]
    }


viewCharacterSummary : Model -> CharacterName -> Maybe (RemoteResource CharacterSummary) -> Document Msg
viewCharacterSummary model characterName mResource =
    { title = characterName ++ " Summary"
    , body =
        [ div [ class "container character-summary" ] <|
            [ header [] [ a [ href "/" ] [ text "< Back to Dashboard" ] ]
            , img [ src <| characterImageUrl characterName 660 460, class "character" ] []
            , h3 [] <| span [ class "character-name" ] [ text characterName ] :: characterSummaryReloader characterName mResource
            ]
                ++ (case mResource of
                        Nothing ->
                            [ span [] [ text "Now loading..." ]
                            , loadingIcon
                            ]

                        Just rr ->
                            viewCharacterSummaryCore characterName rr
                   )
        ]
    }


characterSummaryReloader : CharacterName -> Maybe (RemoteResource CharacterSummary) -> List (Html Msg)
characterSummaryReloader characterName mResource =
    case mResource of
        Nothing ->
            []

        Just rr ->
            if not (RR.hasData rr) && rr.loading then
                []

            else
                [ reloadButton rr.loading <| LoadCharacterSummary characterName ]


viewCharacterSummaryCore : CharacterName -> RemoteResource CharacterSummary -> List (Html Msg)
viewCharacterSummaryCore characterName rr =
    case rr.data of
        Nothing ->
            [ span [] [ text "Now loading..." ]
            , loadingIcon
            ]

        Just (Err _) ->
            [ span [] [ text "Fail loading..." ]
            ]

        Just (Ok data) ->
            [ viewScoreSummary data
            , h1 [] [ text "Score ranking" ]
            , viewCharacterScoreRanking "Hard" data.hard
            , viewCharacterScoreRanking "Normal" data.normal
            ]


viewCharacterScoreRanking : String -> Maybe CharacterSummaryElement -> Html Msg
viewCharacterScoreRanking mode mSummary =
    case mSummary of
        Nothing ->
            div [ class "score-ranking-container" ]
                [ h3 [] [ text mode ]
                , span [] [ text "(No record)" ]
                ]

        Just summary ->
            div [ class "score-ranking-container" ] <|
                h3 [] [ text mode ]
                    :: List.map viewPlayResult summary.scoreRanking


viewPlayResult : PlayResult -> Html Msg
viewPlayResult result =
    div [ class "score-rank-container" ] <| playResultHtmls result


viewPlayResultWithCharacterImage : PlayResult -> Html Msg
viewPlayResultWithCharacterImage result =
    div [ class "score-rank-container" ] <|
        a [ href <| "/character/" ++ result.character ]
            [ div [ class "character-image-container" ]
                [ img [ src <| characterImageUrl result.character 65 65, alt result.character, class "character" ] []
                , span [ class "character-name" ] [ text result.character ]
                ]
            ]
            :: playResultHtmls result


playResultHtmls : PlayResult -> List (Html Msg)
playResultHtmls result =
    [ span [ class <| "score-label " ++ (String.toLower <| Types.gameModeToString result.mode) ] [ text "Score: " ]
    , span [ class <| "score " ++ (String.toLower <| Types.gameModeToString result.mode) ] [ text <| formatComma result.score ]
    , span [ class "play-time" ] [ text <| String.replace "T" " " <| String.dropRight 8 <| result.playTime ]
    , div [ class "armaments-container" ] <| List.map viewArmament result.armaments
    ]


viewArmament : Armament -> Html Msg
viewArmament arm =
    div [ class "armament-container" ]
        [ img [ class "armament", src <| "https://static.time-locker.jabara.info/img/ARM_" ++ String.replace " " "_" arm.name ++ ".png", alt arm.name ] []
        , span [ class "armament-level" ] [ text <| formatComma arm.level ]
        ]


viewScoreSummary : CharacterSummary -> Html Msg
viewScoreSummary summary =
    viewScoreSummaryCore (Maybe.map .scoreSummary summary.hard) (Maybe.map .scoreSummary summary.normal)


viewScoreSummaryCore : Maybe ScoreData -> Maybe ScoreData -> Html Msg
viewScoreSummaryCore mHardScore mNormalScore =
    let
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
                [ th [] []
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


viewNotFound : Model -> Document Msg
viewNotFound _ =
    { title = "Not Found"
    , body = [ h1 [] [ text "Not Found" ] ]
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
    header [] [ h1 [] [ a [ href "/" ] [ text "Time Locker play result" ] ] ]


viewDashboardHeader : Html Msg
viewDashboardHeader =
    div [ class "container" ] <|
        [ h1 [] [ text "Dashboard" ]
        , ul
            []
            [ li [] [ a [ href "/arms/score-per-level", target "time-locker-analyzer-table" ] [ text "Score per armament level" ] ] ]
        ]


viewTotalPlayState : RemoteResource TotalPlayState -> List (Html Msg)
viewTotalPlayState rr =
    let
        fixParts =
            h2 [] [ text "Total play state", reloadButton rr.loading LoadTotalPlayState ]
    in
    case rr.data of
        Nothing ->
            [ fixParts, span [] [ text "Now loading..." ] ]

        Just (Err _) ->
            [ fixParts, span [] [ text "Fail loading characters..." ] ]

        Just (Ok totalPlayScore) ->
            [ fixParts
            , viewScoreSummaryCore (Just totalPlayScore.hard) (Just totalPlayScore.normal)
            ]


viewScoreRanking : RemoteResource ScoreRanking -> List (Html Msg)
viewScoreRanking rr =
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
                    :: (List.map viewPlayResultWithCharacterImage <| List.take 5 scoreRanking.hard)
            , div [ class "score-ranking-container" ] <|
                h3 [] [ text "Normal" ]
                    :: (List.map viewPlayResultWithCharacterImage <| List.take 5 scoreRanking.normal)
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


tagHighScore : Maybe ScoreData -> List (Html.Html msg)
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
