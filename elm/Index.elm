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
    , characterSummaryList : Dict CharacterName (RemoteResource CharacterSummary)
    }


type Msg
    = None
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CharacterListLoaded (Result Http.Error CharacterList)
    | LoadCharacterList
    | GetCharacterListLoadedTime Time.Posix
    | CharactersSortStateChanged SortState
    | CharacterSummaryLoaded CharacterName (Result Http.Error CharacterSummary)
    | LoadCharacterSummary CharacterName


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            parseUrl url

        model =
            { key = key
            , characters = RR.empty
            , page = convPage page
            , charactersSortState = initialSortState
            , characterSummaryList = Dict.empty
            }
    in
    case page of
        DashboardPageW sortState ->
            ( { model
                | characters = RR.startLoading model.characters
                , charactersSortState = sortState
              }
            , Api.getCharacterList CharacterListLoaded
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
        None ->
            ( model, Cmd.none )

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
                    case model.characters.data of
                        Nothing ->
                            let
                                cs =
                                    model.characters
                            in
                            ( { newModel | characters = { cs | loading = True } }
                            , Api.getCharacterList CharacterListLoaded
                            )

                        Just _ ->
                            ( newModel, Cmd.none )

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
                    , Task.perform GetCharacterListLoadedTime Time.now
                    )

                Ok cs ->
                    ( { model | characters = RR.updateSuccessData model.characters <| sortCharacters model.charactersSortState cs }
                    , Task.perform GetCharacterListLoadedTime Time.now
                    )

        LoadCharacterList ->
            ( { model | characters = RR.startLoading model.characters }
            , Api.getCharacterList CharacterListLoaded
            )

        GetCharacterListLoadedTime now ->
            ( { model | characters = RR.updateLastLoadedTime model.characters now }, Cmd.none )

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

        CharacterSummaryLoaded characterName res ->
            case Dict.get characterName model.characterSummaryList of
                Nothing ->
                    ( { model
                        | characterSummaryList =
                            Dict.insert characterName (RR.new res) model.characterSummaryList
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
                    let
                        emp =
                            RR.empty

                        newRr =
                            { emp | loading = True }
                    in
                    ( { model | characterSummaryList = Dict.insert characterName newRr model.characterSummaryList }
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
            viewCharacterList model.characters model.charactersSortState
        ]
    }


viewCharacterSummary : Model -> CharacterName -> Maybe (RemoteResource CharacterSummary) -> Document Msg
viewCharacterSummary model characterName mResource =
    { title = characterName ++ " Summary"
    , body =
        [ div [ class "backdrop-container" ] <| (viewDashboard model).body ++ [ div [ class "backdrop" ] [] ]
        , div [ class "container character-summary" ] <|
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
            , viewScoreRanking "Hard" data.hard
            , viewScoreRanking "Normal" data.normal
            ]


viewScoreRanking : String -> Maybe CharacterSummaryElement -> Html Msg
viewScoreRanking mode mSummary =
    case mSummary of
        Nothing ->
            div [ class "score-ranking-container" ]
                [ h3 [] [ text mode ]
                , span [] [ text "(No record)" ]
                ]

        Just summary ->
            div [ class "score-ranking-container" ] <|
                [ h3 [] [ text mode ] ]
                    ++ List.map viewScoreRank summary.scoreRanking


viewScoreRank : PlayResult -> Html Msg
viewScoreRank rank =
    div [ class "score-rank-container" ]
        [ span [ class "score-label" ] [ text "Score: " ]
        , span [ class "score" ] [ text <| formatComma rank.score ]
        , span [ class "play-time" ] [ text <| String.replace "T" " " <| String.dropRight 8 <| rank.playTime ]
        , div [ class "armaments-container" ] <| List.map viewArmament rank.armaments
        ]


viewArmament : Armament -> Html Msg
viewArmament arm =
    div [ class "armament-container" ]
        [ img [ class "armament", src <| "https://static.time-locker.jabara.info/img/ARM_" ++ String.replace " " "_" arm.name ++ ".png", alt arm.name ] []
        , span [ class "armament-level" ] [ text <| formatComma arm.level ]
        ]


viewScoreSummary : CharacterSummary -> Html Msg
viewScoreSummary summary =
    let
        mHardSummary =
            summary.hard

        mNormalSummary =
            summary.normal

        playCountMapper =
            \sum -> formatComma sum.scoreSummary.playCount

        highScoreMapper =
            \sum -> formatComma sum.scoreSummary.highScore

        averageScoreMapper =
            \sum -> formatComma <| round sum.scoreSummary.averageScore
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
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map playCountMapper mHardSummary ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map playCountMapper mNormalSummary ]
                ]
            , tr []
                [ th [] [ text "High score" ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map highScoreMapper mHardSummary ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map highScoreMapper mNormalSummary ]
                ]
            , tr []
                [ th [] [ text "Average score" ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map averageScoreMapper mHardSummary ]
                , td [ class "number" ] [ text <| Maybe.withDefault "-" <| Maybe.map averageScoreMapper mNormalSummary ]
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


viewCharacterList : RemoteResource CharacterList -> SortState -> List (Html Msg)
viewCharacterList characters sortState =
    case characters.data of
        Nothing ->
            [ h3 [] [ text "Character list", reloadButton characters.loading LoadCharacterList ]
            , span [] [ text "Now loading..." ]
            ]

        Just (Err _) ->
            [ h3 [] [ text "Character list", reloadButton characters.loading LoadCharacterList ]
            , span [] [ text "Fail loading characters..." ]
            ]

        Just (Ok cs) ->
            h3 [] [ text "Character list", reloadButton characters.loading LoadCharacterList ]
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


tagScore : CharacterListElement -> Html.Html msg
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


sortCore : SortOrder -> (CharacterListElement -> number) -> CharacterList -> CharacterList
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
