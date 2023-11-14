module Shared.Msg exposing (Msg(..))

{-| -}

import Auth.Common exposing (UserInfo)
import Shared.Model exposing (LoginState)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = GotNewSmashedLikes Int
    | GoogleSigninRequested
    | AuthLogOut
    | UserLoginUpdated LoginState
    | UpdateSharedModel Shared.Model.Model
    | AuthSuccess UserInfo
