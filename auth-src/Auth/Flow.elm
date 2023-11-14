module Auth.Flow exposing (..)

import Auth.Common exposing (LogoutEndpointConfig(..), MethodId, ToBackend(..))
import Auth.Method.EmailMagicLink
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
import Auth.Protocol.OAuth
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import List.Extra as List
import OAuth
import OAuth.AuthorizationCode as OAuth
import Process
import SHA1
import Task
import Time
import Url exposing (Protocol(..), Url)
import Url.Builder exposing (QueryParameter)


init :
    { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> Url
    -> Navigation.Key
    -> (Auth.Common.ToBackend -> Cmd frontendMsg)
    -> ( { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }, Cmd frontendMsg )
init model methodId origin navigationKey toBackendFn =
    case methodId of
        "EmailMagicLink" ->
            Auth.Method.EmailMagicLink.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        "OAuthGithub" ->
            Auth.Protocol.OAuth.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        "OAuthGoogle" ->
            Auth.Protocol.OAuth.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        "OAuthAuth0" ->
            Auth.Protocol.OAuth.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        _ ->
            let
                clearUrl =
                    Navigation.replaceUrl navigationKey (Url.toString model.authRedirectBaseUrl)
            in
            ( { model | authFlow = Auth.Common.Errored <| Auth.Common.ErrAuthString ("Unsupported auth method: " ++ methodId) }
            , clearUrl
            )


onFrontendLogoutCallback navigationMsg =
    navigationMsg


updateFromFrontend { asBackendMsg } clientId sessionId authToBackend model =
    case authToBackend of
        Auth.Common.AuthSigninInitiated params ->
            ( model
            , withCurrentTime
                (\now ->
                    asBackendMsg <|
                        Auth.Common.AuthSigninInitiated_
                            { sessionId = sessionId
                            , clientId = clientId
                            , methodId = params.methodId
                            , baseUrl = params.baseUrl
                            , now = now
                            , username = params.username
                            }
                )
            )

        Auth.Common.AuthCallbackReceived methodId receivedUrl code state ->
            ( model
            , Time.now
                |> Task.perform
                    (\now ->
                        asBackendMsg <|
                            Auth.Common.AuthCallbackReceived_
                                sessionId
                                clientId
                                methodId
                                receivedUrl
                                code
                                state
                                now
                    )
            )

        Auth.Common.AuthRenewSessionRequested ->
            ( model
            , Time.now
                |> Task.perform
                    (\t ->
                        asBackendMsg <|
                            Auth.Common.AuthRenewSession sessionId clientId
                    )
            )

        Auth.Common.AuthLogoutRequested ->
            ( model
            , Time.now
                |> Task.perform
                    (\t ->
                        asBackendMsg <|
                            Auth.Common.AuthLogout sessionId clientId
                    )
            )


type alias BackendUpdateConfig frontendMsg backendMsg toFrontend frontendModel backendModel =
    { asToFrontend : Auth.Common.ToFrontend -> toFrontend
    , asBackendMsg : Auth.Common.BackendMsg -> backendMsg
    , sendToFrontend : Auth.Common.SessionId -> toFrontend -> Cmd backendMsg
    , backendModel : { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }
    , loadMethod : Auth.Common.MethodId -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel)
    , handleAuthSuccess :
        Auth.Common.SessionId
        -> Auth.Common.ClientId
        -> Auth.Common.UserInfo
        -> MethodId
        -> Maybe Auth.Common.Token
        -> Time.Posix
        -> ( { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }, Cmd backendMsg )
    , renewSession : Auth.Common.SessionId -> Auth.Common.ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    , logout : Auth.Common.SessionId -> Auth.Common.ClientId -> backendModel -> ( backendModel, Cmd backendMsg )
    , isDev : Bool
    }


backendUpdate :
    BackendUpdateConfig
        frontendMsg
        backendMsg
        toFrontend
        frontendModel
        { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }
    -> Auth.Common.BackendMsg
    -> ( { backendModel | pendingAuths : Dict Auth.Common.SessionId Auth.Common.PendingAuth }, Cmd backendMsg )
backendUpdate { asToFrontend, asBackendMsg, sendToFrontend, backendModel, loadMethod, handleAuthSuccess, renewSession, logout, isDev } authBackendMsg =
    let
        authError str =
            asToFrontend (Auth.Common.AuthError (Auth.Common.ErrAuthString str))

        withMethod methodId clientId fn =
            case loadMethod methodId of
                Nothing ->
                    ( backendModel
                    , sendToFrontend clientId <| authError ("Unsupported auth method: " ++ methodId)
                    )

                Just method ->
                    fn method
    in
    case authBackendMsg of
        Auth.Common.AuthSigninInitiated_ { sessionId, clientId, methodId, baseUrl, now, username } ->
            withMethod methodId
                clientId
                (\method ->
                    case method of
                        Auth.Common.ProtocolEmailMagicLink config ->
                            config.initiateSignin sessionId clientId backendModel { username = username } now

                        Auth.Common.ProtocolOAuth config ->
                            Auth.Protocol.OAuth.initiateSignin isDev sessionId baseUrl config asBackendMsg now backendModel
                )

        Auth.Common.AuthSigninInitiatedDelayed_ sessionId initiateMsg ->
            ( backendModel, sendToFrontend sessionId (asToFrontend initiateMsg) )

        Auth.Common.AuthCallbackReceived_ sessionId clientId methodId receivedUrl code state now ->
            withMethod methodId
                clientId
                (\method ->
                    case method of
                        Auth.Common.ProtocolEmailMagicLink config ->
                            config.onAuthCallbackReceived sessionId clientId receivedUrl code state now asBackendMsg backendModel

                        Auth.Common.ProtocolOAuth config ->
                            Auth.Protocol.OAuth.onAuthCallbackReceived sessionId clientId config receivedUrl code state now asBackendMsg backendModel
                )

        Auth.Common.AuthSuccess sessionId clientId methodId now res ->
            let
                removeSession backendModel_ =
                    { backendModel_ | pendingAuths = backendModel_.pendingAuths |> Dict.remove sessionId }
            in
            withMethod methodId
                clientId
                (\method ->
                    case res of
                        Ok ( userInfo, authToken ) ->
                            handleAuthSuccess sessionId clientId userInfo methodId authToken now
                                |> Tuple.mapFirst removeSession

                        Err err ->
                            ( backendModel, sendToFrontend sessionId (asToFrontend <| Auth.Common.AuthError err) )
                )

        Auth.Common.AuthRenewSession sessionId clientId ->
            renewSession sessionId clientId backendModel

        Auth.Common.AuthLogout sessionId clientId ->
            logout sessionId clientId backendModel


signInRequested :
    Auth.Common.MethodId
    -> { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }
    -> Maybe String
    -> ( { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }, Auth.Common.ToBackend )
signInRequested methodId model username =
    ( { model | authFlow = Auth.Common.Requested methodId }
    , Auth.Common.AuthSigninInitiated { methodId = methodId, baseUrl = model.authRedirectBaseUrl, username = username }
    )


signOutRequested :
    Maybe LogoutEndpointConfig
    -> List QueryParameter
    -> { a | authFlow : Auth.Common.Flow, authLogoutReturnUrlBase : Url }
    -> ( { a | authFlow : Auth.Common.Flow, authLogoutReturnUrlBase : Url }, Cmd msg )
signOutRequested maybeUrlConfig callBackQueries model =
    ( { model | authFlow = Auth.Common.Idle }
    , case maybeUrlConfig of
        Just (Tenant urlConfig) ->
            Navigation.load <|
                Url.toString urlConfig.url
                    ++ Url.toString model.authLogoutReturnUrlBase
                    ++ urlConfig.returnPath
                    ++ Url.Builder.toQuery callBackQueries

        Just (Home homeUrlConfig) ->
            Navigation.load <|
                Url.toString model.authLogoutReturnUrlBase
                    ++ homeUrlConfig.returnPath
                    ++ Url.Builder.toQuery callBackQueries

        Nothing ->
            Navigation.load <|
                Url.toString model.authLogoutReturnUrlBase
                    ++ Url.Builder.toQuery callBackQueries
    )


startProviderSignin :
    Url
    -> { frontendModel | authFlow : Auth.Common.Flow }
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Cmd msg )
startProviderSignin url model =
    ( { model | authFlow = Auth.Common.Pending }
    , Navigation.load (Url.toString url)
    )


setError :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> Auth.Common.Error
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Cmd msg )
setError model err =
    setAuthFlow model <| Auth.Common.Errored err


setAuthFlow :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> Auth.Common.Flow
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Cmd msg )
setAuthFlow model flow =
    ( { model | authFlow = flow }, Cmd.none )


errorToString : Auth.Common.Error -> String
errorToString error =
    case error of
        Auth.Common.ErrStateMismatch ->
            "ErrStateMismatch"

        Auth.Common.ErrAuthorization authorizationError ->
            "ErrAuthorization"

        Auth.Common.ErrAuthentication authenticationError ->
            "ErrAuthentication"

        Auth.Common.ErrHTTPGetAccessToken ->
            "ErrHTTPGetAccessToken"

        Auth.Common.ErrHTTPGetUserInfo ->
            "ErrHTTPGetUserInfo"

        Auth.Common.ErrAuthString err ->
            err


withCurrentTime fn =
    Time.now |> Task.perform fn


methodLoader : List (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel) -> Auth.Common.MethodId -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel)
methodLoader methods methodId =
    methods
        |> List.find
            (\config ->
                case config of
                    Auth.Common.ProtocolEmailMagicLink method ->
                        method.id == methodId

                    Auth.Common.ProtocolOAuth method ->
                        method.id == methodId
            )


findMethod :
    Auth.Common.MethodId
    -> Auth.Common.Config frontendMsg toBackend backendMsg toFrontend frontendModel backendModel
    -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel)
findMethod methodId config =
    methodLoader config.methods methodId
