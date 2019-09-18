module RemoteResource exposing (RemoteResource, empty, emptyLoading, finishLoading, hasData, loadIfNecessary, new, resourceValue, startLoading, updateData, updateSuccessData, withDummyData)

import Http
import Time


type alias RemoteResource a =
    { data : Maybe (Result Http.Error a)
    , dummyData : Maybe a
    , loading : Bool
    }


empty : RemoteResource a
empty =
    { data = Nothing, dummyData = Nothing, loading = False }


emptyLoading : RemoteResource a
emptyLoading =
    { empty | loading = True }


withDummyData : a -> RemoteResource a
withDummyData dummyData =
    { empty | dummyData = Just dummyData }


new : Result Http.Error a -> RemoteResource a
new res =
    { empty | data = Just res }


resourceValue : RemoteResource a -> b -> (a -> b) -> b
resourceValue rr defaultValue operation =
    case rr.data of
        Nothing ->
            defaultValue

        Just (Err _) ->
            defaultValue

        Just (Ok val) ->
            operation val


updateData : RemoteResource a -> Result Http.Error a -> RemoteResource a
updateData rr newData =
    { rr | data = Just newData, loading = False, dummyData = Nothing }


updateSuccessData : RemoteResource a -> a -> RemoteResource a
updateSuccessData rr newData =
    { rr | data = Just <| Ok newData, loading = False, dummyData = Nothing }


startLoading : RemoteResource a -> RemoteResource a
startLoading rr =
    { rr | loading = True }


finishLoading : RemoteResource a -> RemoteResource a
finishLoading rr =
    { rr | loading = False }


hasData : RemoteResource a -> Bool
hasData =
    Maybe.withDefault False << Maybe.map (\_ -> True) << .data


loadIfNecessary : RemoteResource a -> Cmd msg -> ( RemoteResource a, Cmd msg )
loadIfNecessary rr cmd =
    if hasData rr then
        ( rr, Cmd.none )

    else
        ( startLoading rr, cmd )
