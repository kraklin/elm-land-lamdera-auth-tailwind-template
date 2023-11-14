module Types exposing (..)

import Auth.Common exposing (UserInfo)
import Bridge
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Main as ElmLand
import Url exposing (Url)


type alias FrontendModel =
    ElmLand.Model


type alias BackendModel =
    { smashedLikes : Int
    , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
    , sessions : Dict SessionId UserInfo
    }


type alias FrontendMsg =
    ElmLand.Msg


type alias ToBackend =
    Bridge.ToBackend


type BackendMsg
    = OnConnect SessionId ClientId
    | AuthBackendMsg Auth.Common.BackendMsg


type ToFrontend
    = NewSmashedLikes Int
    | AuthToFrontend Auth.Common.ToFrontend
    | AuthSuccess UserInfo
    | UserInfo (Maybe UserInfo)
