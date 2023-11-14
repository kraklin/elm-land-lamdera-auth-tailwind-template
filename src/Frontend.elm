module Frontend exposing (..)

import Auth.Flow
import Bridge
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Json.Encode
import Lamdera
import LamderaAuth
import Main as ElmLand
import Pages.Home_
import Route.Path
import Shared.Model exposing (LoginState(..))
import Shared.Msg
import Task
import Time
import Types exposing (..)
import Url exposing (Url)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = ElmLand.UrlRequested
        , onUrlChange = ElmLand.UrlChanged
        , update = ElmLand.update
        , updateFromBackend = updateFromBackend
        , subscriptions = ElmLand.subscriptions
        , view = ElmLand.view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        ( model, cmds ) =
            ElmLand.init Json.Encode.null url key

        ( sharedModel, sharedCmd ) =
            authCallbackCmd model.shared url key
    in
    ( { model | shared = sharedModel }
    , Cmd.batch
        [ cmds
        , sharedCmd
        ]
    )


authCallbackCmd : Shared.Model.Model -> Url -> Nav.Key -> ( Shared.Model.Model, Cmd FrontendMsg )
authCallbackCmd model url key =
    let
        { path } =
            url
    in
    case path of
        "/login/OAuthGoogle/callback" ->
            callbackForGoogleAuth model url key

        _ ->
            ( model, Cmd.none )


callbackForGoogleAuth : Shared.Model.Model -> Url -> Nav.Key -> ( Shared.Model.Model, Cmd FrontendMsg )
callbackForGoogleAuth model url key =
    let
        ( authM, authCmd ) =
            Auth.Flow.init model
                "OAuthGoogle"
                url
                key
                (\msg -> Lamdera.sendToBackend (Bridge.AuthToBackend msg))
    in
    ( authM, authCmd )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewSmashedLikes smashedLikes ->
            ( model, sendSharedMsg <| Shared.Msg.GotNewSmashedLikes smashedLikes )

        AuthToFrontend authMsg ->
            let
                ( newShared, cmd ) =
                    LamderaAuth.updateFromBackend authMsg model.shared
            in
            ( model, Cmd.batch [ sendSharedMsg <| Shared.Msg.UpdateSharedModel newShared, cmd ] )

        AuthSuccess userInfo ->
            ( model
            , Cmd.batch
                [ sendSharedMsg <| Shared.Msg.AuthSuccess userInfo
                ]
            )

        UserInfo mUserInfo ->
            let
                loginState =
                    case mUserInfo of
                        Just userInfo ->
                            LoggedIn userInfo

                        Nothing ->
                            NotLogged
            in
            ( model, sendSharedMsg <| Shared.Msg.UserLoginUpdated loginState )


sendSharedMsg : Shared.Msg.Msg -> Cmd FrontendMsg
sendSharedMsg msg =
    Time.now |> Task.perform (always (ElmLand.Shared msg))
