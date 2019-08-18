module RemoteResource exposing (RemoteResource, empty, finishLoading, new, resourceValue, startLoading, updateData, updateLastLoadedTime)

import Http
import Time


type alias RemoteResource a =
    { data : Maybe (Result Http.Error a)
    , loading : Bool
    , lastLoadedTime : Maybe Time.Posix
    }


empty : RemoteResource a
empty =
    { data = Nothing, loading = False, lastLoadedTime = Nothing }


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
    { rr | data = Just newData, loading = False }


startLoading : RemoteResource a -> RemoteResource a
startLoading rr =
    { rr | loading = True }


finishLoading : RemoteResource a -> RemoteResource a
finishLoading rr =
    { rr | loading = False, lastLoadedTime = Nothing }


updateLastLoadedTime : RemoteResource a -> Time.Posix -> RemoteResource a
updateLastLoadedTime rr newTime =
    { rr | lastLoadedTime = Just newTime }
