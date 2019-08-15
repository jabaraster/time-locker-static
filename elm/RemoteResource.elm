module RemoteResource exposing (..)

import Http
import Time


type alias RemoteResource a =
    { data : Maybe (Result Http.Error a)
    , loading : Bool
    , lastLoadedTime : Maybe Time.Posix
    }


emptyRemoteResource =
    { data = Nothing, loading = False, lastLoadedTime = Nothing }


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
