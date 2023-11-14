module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Auth.Common exposing (Flow(..))
import Auth.Flow
import Bridge exposing (ToBackend(..))
import Dict
import Effect exposing (Effect)
import Json.Decode
import Lamdera
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (LoginState(..))
import Shared.Msg



-- FLAGS


type alias Flags =
    {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed {}



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        url =
            route.url
    in
    ( { smashedLikes = 0
      , login = NotAsked
      , authFlow = Idle
      , authRedirectBaseUrl = { url | query = Nothing, fragment = Nothing }
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.GotNewSmashedLikes count ->
            ( { model | smashedLikes = count }
            , Effect.none
            )

        Shared.Msg.GoogleSigninRequested ->
            Auth.Flow.signInRequested "OAuthGoogle" model Nothing
                |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend >> Effect.sendCmd)

        Shared.Msg.AuthLogOut ->
            ( { model | login = NotLogged }, Lamdera.sendToBackend LogOut |> Effect.sendCmd )

        Shared.Msg.UserLoginUpdated loginStatus ->
            ( { model | login = loginStatus }, Effect.none )

        Shared.Msg.UpdateSharedModel newModel ->
            ( newModel, Effect.none )

        Shared.Msg.AuthSuccess userInfo ->
            ( { model | login = LoggedIn userInfo }
            , Effect.pushRoute
                { path = Route.Path.Dashboard
                , query = Dict.empty
                , hash = Nothing
                }
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
