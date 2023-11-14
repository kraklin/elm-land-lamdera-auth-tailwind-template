module Shared.Model exposing (LoginState(..), Model)

import Auth.Common exposing (UserInfo)
import Url exposing (Url)


{-| -}
type LoginState
    = NotAsked
    | NotLogged
    | LoginTokenSent
    | LoggedIn UserInfo


type alias Model =
    { smashedLikes : Int
    , login : LoginState
    , authFlow : Auth.Common.Flow
    , authRedirectBaseUrl : Url
    }
