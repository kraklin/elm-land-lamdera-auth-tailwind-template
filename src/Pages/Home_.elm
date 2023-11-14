module Pages.Home_ exposing (Model, Msg(..), page)

import Auth.Flow
import Bridge
import Effect exposing (..)
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events as Events exposing (onClick)
import Lamdera
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Shared.Msg
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = SmashedLikeButton
    | Login
    | LogOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SmashedLikeButton ->
            ( model
            , Effect.sendCmd <| Lamdera.sendToBackend Bridge.SmashedLikeButton
            )

        Login ->
            ( model
            , Effect.login
            )

        LogOut ->
            ( model
            , Effect.logout
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Elm Land ❤️ Lamdera"
    , body =
        [ Html.div
            [ Attrs.class "flex min-h-full flex-col justify-center py-12 sm:px-6 lg:px-8"
            ]
            [ Html.div
                [ Attrs.class "sm:mx-auto sm:w-full sm:max-w-md"
                ]
                [ Html.img
                    [ Attrs.class "mx-auto h-10 w-auto"
                    , Attrs.src "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600"
                    , Attrs.alt "Your Company"
                    ]
                    []
                , Html.h2
                    [ Attrs.class "mt-6 text-center text-2xl font-bold leading-9 tracking-tight text-gray-900"
                    ]
                    [ Html.text "Sign in to your account" ]
                ]
            , Html.div
                [ Attrs.class "mt-10 sm:mx-auto sm:w-full sm:max-w-[480px]"
                ]
                [ Html.div
                    [ Attrs.class "bg-white px-6 py-12 shadow sm:rounded-lg sm:px-12"
                    ]
                    [ Html.div []
                        [ Html.div
                            [ Attrs.class "mt-6 flex justify-center"
                            ]
                            [ Html.button
                                [ Attrs.href "#"
                                , Attrs.class "flex items-center justify-center gap-3 rounded-full px-3 py-1.5 border border-[#1F1F1F] focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#24292F] hover:bg-gray-100"
                                , Events.onClick Login
                                ]
                                [ Html.img [Attrs.src "google-logo.svg"] []
                                , Html.span
                                    [ Attrs.class "text-sm text-[#1F1F1F] font-semibold leading-6"
                                    ]
                                    [ Html.text "Sign in with Google" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }
