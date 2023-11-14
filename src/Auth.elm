module Auth exposing (User, onPageLoad, viewLoadingPage)

import Auth.Action
import Auth.Common exposing (UserInfo)
import Dict
import Route exposing (Route)
import Route.Path
import Shared.Model as Shared exposing (LoginState(..))
import View exposing (View)


type alias User =
    UserInfo


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.login of
        LoggedIn user ->
            Auth.Action.loadPageWithUser user

        NotAsked ->
            Auth.Action.showLoadingPage <| viewLoadingPage shared route

        _ ->
            Auth.Action.pushRoute
                { path = Route.Path.Home_
                , query =
                    Dict.fromList
                        [ ( "from", route.url.path )
                        ]
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.showLoadingPage` is returned from `onPageLoad`.
-}
viewLoadingPage : Shared.Model -> Route () -> View Never
viewLoadingPage shared route =
    View.fromString "Loading..."
