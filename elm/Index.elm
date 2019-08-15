module Index exposing (CharacterName, Model, Msg(..), Page(..), characterImageUrl, init, main, parseUrl, subscriptions, update, view, viewCharacterSummary, viewHome, viewNotFound)

import Api
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import RemoteResource exposing (..)
import Task
import Time
import Types
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


type alias CharacterName =
    String


type Page
    = HomePage
    | NotFoundPage
    | CharacterSummaryPage CharacterName


type alias Model =
    { key : Nav.Key
    , page : Page
    , characters : RemoteResource (List Types.Character)
    }


type Msg
    = None
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CharacterListLoaded (Result Http.Error (List Types.Character))
    | LoadCharacterList
    | GetCharacterListLoadedTime Time.Posix


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            parseUrl url

        model =
            { key = key
            , page = page
            , characters = emptyRemoteResource
            }
    in
    case page of
        HomePage ->
            ( { model | characters = startLoading model.characters }
            , Api.getCharacterList CharacterListLoaded
            )

        NotFoundPage ->
            ( model, Cmd.none )

        CharacterSummaryPage name ->
            ( { model | page = CharacterSummaryPage <| Maybe.withDefault name <| Url.percentDecode name }
            , Cmd.none
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

                CharacterSummaryPage name ->
                    ( { newModel | page = CharacterSummaryPage <| Maybe.withDefault name <| Url.percentDecode name }
                    , Cmd.none
                    )

        CharacterListLoaded res ->
            ( { model | characters = updateData model.characters res }
            , Task.perform GetCharacterListLoadedTime Time.now
            )

        LoadCharacterList ->
            ( { model | characters = startLoading model.characters }
            , Api.getCharacterList CharacterListLoaded
            )

        GetCharacterListLoadedTime now ->
            ( { model | characters = updateLastLoadedTime model.characters now }, Cmd.none )


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
                    viewCharacterSummary name model

        loadingIcon =
            if isLoading model then
                [ span [ class "fas fa-spinner loading loading-icon" ] [] ]

            else
                []
    in
    { title = doc.title ++ " | Jabara's Time Locker Analyzing"
    , body = loadingIcon ++ doc.body
    }


viewCharacterSummary : String -> Model -> Document Msg
viewCharacterSummary characterName _ =
    { title = characterName ++ " Summary"
    , body =
        [ div [ class "container character-summary" ]
            [ header [] [ a [ href "/" ] [ text "< Back to Dashboard" ] ]
            , img [ src <| characterImageUrl characterName 660 460, class "character" ] []
            , h3 [] [ span [ class "character-name" ] [ text <| characterName ] ]
            ]
        ]
    }


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
                    [ span [] [ text "Character list is loading now." ]
                    , span [ class "fas fa-sync loading" ] []
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
                                a [ href <| "/character/" ++ character.name ]
                                    [ div [ class "character-image-container-floating" ]
                                        [ img [ src <| characterImageUrl character.name 65 65, alt character.name, class "character" ] []
                                        , span [ class "character-name" ] [ text character.name ]
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
                , li [] [ a [ href "/score/character-high", target "time-locker-analyzer-table" ] [ text "Character highscore" ] ]
                , li [] [ a [ href "/arms/score-per-level", target "time-locker-analyzer-table" ] [ text "Score per armament level" ] ]
                ]
            ]
                ++ [ hr [] [] ]
                ++ tags
        ]
    }


characterImageUrl : CharacterName -> Int -> Int -> String
characterImageUrl characterName width height =
    "https://static.time-locker.jabara.info/img/" ++ characterName ++ "@" ++ String.fromInt width ++ "x" ++ String.fromInt height ++ ".png"
