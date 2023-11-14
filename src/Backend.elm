module Backend exposing (..)

import Auth.Common
import Auth.Flow
import Bridge exposing (..)
import Dict
import Html
import Lamdera exposing (ClientId, SessionId)
import LamderaAuth
import Types exposing (BackendModel, BackendMsg(..), ToFrontend(..))


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Lamdera.onConnect OnConnect
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { smashedLikes = 0
      , pendingAuths = Dict.empty
      , sessions = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect sid cid ->
            ( model
            , Lamdera.sendToFrontend cid <| UserInfo (findUser sid model)
            )

        AuthBackendMsg authMsg ->
            Auth.Flow.backendUpdate (LamderaAuth.backendConfig model) authMsg


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        SmashedLikeButton ->
            let
                newSmashedLikes =
                    model.smashedLikes + 1
            in
            ( { model | smashedLikes = newSmashedLikes }, Lamdera.broadcast <| NewSmashedLikes newSmashedLikes )

        AuthToBackend authMsg ->
            Auth.Flow.updateFromFrontend (LamderaAuth.backendConfig model) clientId sessionId authMsg model

        GetUser ->
            ( model, Lamdera.sendToFrontend clientId <| UserInfo (findUser sessionId model) )

        LogOut ->
            LamderaAuth.logout sessionId clientId model


findUser : SessionId -> Model -> Maybe Auth.Common.UserInfo
findUser sessionId model =
    Dict.get sessionId model.sessions
