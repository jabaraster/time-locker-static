module Index exposing (Model, Msg(..), Page(..), characterImageUrl, init, main, parseUrl, subscriptions, update, view, viewCharacterSummary, viewHome, viewNotFound)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import List.Split as LS
import RemoteResource as RR exposing (RemoteResource)
import Task
import Time
import Types exposing (..)
import Url
import Url.Parser exposing (..)


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


type Page
    = HomePage
    | NotFoundPage
    | CharacterSummaryPage CharacterName


type alias Model =
    { key : Nav.Key
    , page : Page
    , characters : RemoteResource CharacterList
    , characterSummaryList : Dict CharacterName (RemoteResource CharacterSummary)
    }


type Msg
    = None
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CharacterListLoaded (Result Http.Error CharacterList)
    | LoadCharacterList
    | GetCharacterListLoadedTime Time.Posix
    | CharacterSummaryLoaded CharacterName (Result Http.Error CharacterSummary)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            parseUrl url

        model =
            { key = key
            , page = page
            , characters = RR.empty
            , characterSummaryList = Dict.empty
            }
    in
    case page of
        HomePage ->
            ( { model | characters = RR.startLoading model.characters }
            , Api.getCharacterList CharacterListLoaded
            )

        NotFoundPage ->
            ( model, Cmd.none )

        CharacterSummaryPage name ->
            let
                characterName =
                    Maybe.withDefault name <| Url.percentDecode name
            in
            ( { model | page = CharacterSummaryPage characterName }
            , Api.getCharacterSummary characterName <| CharacterSummaryLoaded characterName
            )


parseUrl : Url.Url -> Page
parseUrl url =
    Maybe.withDefault NotFoundPage <|
        Url.Parser.parse
            (Url.Parser.oneOf
                [ Url.Parser.map HomePage <| Url.Parser.top
                , Url.Parser.map CharacterSummaryPage (Url.Parser.s "character" </> Url.Parser.string)
                ]
            )
            url


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
                    { model | page = page }
            in
            case page of
                HomePage ->
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
                            ( { newModel | page = CharacterSummaryPage characterName }
                            , Api.getCharacterSummary characterName <| CharacterSummaryLoaded characterName
                            )

                        Just inList ->
                            ( { newModel | page = CharacterSummaryPage characterName }
                            , Cmd.none
                            )

        CharacterListLoaded res ->
            ( { model | characters = RR.updateData model.characters res }
            , Task.perform GetCharacterListLoadedTime Time.now
            )

        LoadCharacterList ->
            ( { model | characters = RR.startLoading model.characters }
            , Api.getCharacterList CharacterListLoaded
            )

        GetCharacterListLoadedTime now ->
            ( { model | characters = RR.updateLastLoadedTime model.characters now }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


isLoading : Model -> Bool
isLoading model =
    model.characters.loading


view : Model -> Document Msg
view model =
    let
        doc =
            case model.page of
                HomePage ->
                    viewHome model

                NotFoundPage ->
                    viewNotFound model

                CharacterSummaryPage name ->
                    case Dict.get name model.characterSummaryList of
                        Nothing ->
                            viewCharacterSummary name <| Nothing

                        Just inList ->
                            viewCharacterSummary name <| Just inList

        loading =
            if isLoading model then
                [ span [ class "fas fa-spinner loading loading-icon" ] [] ]

            else
                []
    in
    { title = doc.title ++ " | Jabara's Time Locker Analyzing"
    , body = loading ++ doc.body
    }


viewCharacterSummary : CharacterName -> Maybe (RemoteResource CharacterSummary) -> Document Msg
viewCharacterSummary characterName mResource =
    { title = characterName ++ " Summary"
    , body =
        [ div [ class "container character-summary" ] <|
            [ header [] [ a [ href "/" ] [ text "< Back to Dashboard" ] ]
            , img [ src <| characterImageUrl characterName 660 460, class "character" ] []
            , h3 [] [ span [ class "character-name" ] [ text <| characterName ] ]
            ]
                ++ (case mResource of
                        Nothing ->
                            [ span [] [ text "now loading..." ]
                            , loadingIcon
                            ]

                        Just rr ->
                            viewCharacterSummaryCore rr
                   )
        ]
    }


viewCharacterSummaryCore : RemoteResource CharacterSummary -> List (Html Msg)
viewCharacterSummaryCore rr =
    case rr.data of
        Nothing ->
            [ span [] [ text "now loading..." ]
            , loadingIcon
            ]

        Just (Err _) ->
            [ span [] [ text "Fail loading..." ] ]

        Just (Ok data) ->
            viewCharacterSummaryElement "Hard" data.hard
                ++ viewCharacterSummaryElement "Normal" data.normal


viewCharacterSummaryElement : String -> Maybe CharacterSummaryElement -> List (Html Msg)
viewCharacterSummaryElement mode mSummary =
    case mSummary of
        Nothing ->
            [ h3 [] [ text mode ], text "-" ]

        Just summary ->
            [ div [ class "character-summary-container" ]
                [ h1 [] [ text mode ]
                , table []
                    [ tbody [ class "summary" ]
                        [ tr []
                            [ th [] [ text "Play count" ]
                            , td [ class "number" ] [ text <| formatComma summary.scoreSummary.playCount ]
                            ]
                        , tr []
                            [ th [] [ text "High score" ]
                            , td [ class "number" ] [ text <| formatComma summary.scoreSummary.highScore ]
                            ]
                        , tr []
                            [ th [] [ text "Average score" ]
                            , td [ class "number" ] [ text <| formatComma <| round summary.scoreSummary.averageScore ]
                            ]
                        ]
                    ]
                , hr [] []
                , h4 [] [ text "Higher score result" ]
                , table [ class "armament-level-list" ]
                    [ tbody [] <|
                        List.concat <|
                            List.map
                                (\rank ->
                                    [ tr [ class "basic-play-result" ]
                                        [ th [ colspan 2 ] [ span [] [ text "Score" ] ]
                                        , td [ colspan 2, class "number" ] [ span [] [ text <| formatComma rank.score ] ]
                                        , td [ colspan 2, class "play-time" ] [ span [] [ text rank.playTime ] ]
                                        ]
                                    ]
                                        ++ (List.map viewArmaments <| LS.chunksOfLeft 2 rank.armaments)
                                )
                                summary.scoreRanking
                    ]
                ]
            ]


viewArmaments : List Armament -> Html Msg
viewArmaments arms =
    tr [] <|
        List.concat <|
            List.map viewArmament arms


viewArmament : Armament -> List (Html Msg)
viewArmament arm =
    [ td [ colspan 3, class "armament-level-container" ]
        [ img [ src <| "https://static.time-locker.jabara.info/img/ARM_" ++ String.replace " " "_" arm.name ++ ".png" ] []
        , span [ class "armament-name" ] [ text <| arm.name ]
        ]
    , td [ colspan 3, class "armament-level-container" ] [ span [ class "armament-level" ] [ text <| String.fromInt arm.level ] ]
    ]


viewNotFound : Model -> Document Msg
viewNotFound _ =
    { title = "Not Found"
    , body = [ h1 [] [ text "Not Found" ] ]
    }


viewHome : Model -> Document Msg
viewHome model =
    let
        msg =
            "Dashboard"

        tags =
            case model.characters.data of
                Nothing ->
                    [ span [] [ text "Now loading..." ]
                    , loadingIcon
                    ]

                Just (Ok cs) ->
                    div []
                        [ button
                            [ class "btn btn-default"
                            , onClick LoadCharacterList
                            ]
                            [ i [ classList [ ( "fas fa-sync", True ), ( "loading", model.characters.loading ) ] ] [] ]
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

                Just (Err _) ->
                    [ span [] [ text "Fail loading characters..." ] ]
    in
    { title = msg
    , body =
        [ div [ class "container" ] <|
            [ h1 [] [ text msg ]
            , ul
                []
                [ li [] [ a [ href "/score/character-average", target "time-locker-analyzer-table" ] [ text "Character average score" ] ]
                , li [] [ a [ href "/score/character-high", target "time-locker-analyzer-table" ] [ text "Character high score" ] ]
                , li [] [ a [ href "/arms/score-per-level", target "time-locker-analyzer-table" ] [ text "Score per armament level" ] ]
                ]
            ]
                ++ [ hr [] [] ]
                ++ tags
        ]
    }


tagScore : CharacterListElement -> Html.Html msg
tagScore c =
    table [ class "character-list" ]
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
        , td [] [ text <| Maybe.withDefault "-" <| Maybe.map (\score -> formatComma score.playCount) mScore ]
        ]
    , tr [ class "score-element" ]
        [ th [] [ text "High score" ]
        , td [] [ text <| Maybe.withDefault "-" <| Maybe.map (\score -> formatComma score.highScore) mScore ]
        ]
    , tr [ class "score-element" ]
        [ th [] [ text "Average score" ]
        , td [] [ text <| Maybe.withDefault "-" <| Maybe.map (\score -> formatComma <| round score.averageScore) mScore ]
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
