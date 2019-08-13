module Index exposing (Model, Msg(..), init, main, onUrlChange, onUrlRequest, subscriptions, view)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Url exposing (Url)



-- MAIN


main : Platform.Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type alias Model =
    {}


type Msg
    = None


init : frags -> Url -> Key -> ( Model, Cmd msg )
init _ _ _ =
    ( {}, Cmd.none )


view : Model -> Document Msg
view _ =
    let
        msg =
            "Analyzing of Jabara's Time Locker Play Result"
    in
    { title = msg
    , body = [ h1 [ class "h1" ] [ text msg ] ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ msg =
    ( msg, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    None


onUrlChange : Url -> Msg
onUrlChange _ =
    None
