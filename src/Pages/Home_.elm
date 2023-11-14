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
import Svg
import Svg.Attributes as SvgAttrs
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
                            [ Attrs.class "mt-6 grid grid-cols-1 gap-4"
                            ]
                            [ Html.button
                                [ Attrs.href "#"
                                , Attrs.class "flex w-full items-center justify-center gap-3 rounded-md bg-[#24292F] px-3 py-1.5 text-white focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-[#24292F]"
                                , Events.onClick Login
                                ]
                                [ Svg.svg
                                    [ SvgAttrs.class "h-5 w-5"
                                    , SvgAttrs.fill "currentColor"
                                    , SvgAttrs.viewBox "0 0 20 20"
                                    , Attrs.attribute "aria-hidden" "true"
                                    ]
                                    [ Svg.path
                                        [ SvgAttrs.fillRule "evenodd"
                                        , SvgAttrs.d "M10 0C4.477 0 0 4.484 0 10.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0110 4.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.203 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.942.359.31.678.921.678 1.856 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0020 10.017C20 4.484 15.522 0 10 0z"
                                        , SvgAttrs.clipRule "evenodd"
                                        ]
                                        []
                                    ]
                                , Html.span
                                    [ Attrs.class "text-sm font-semibold leading-6"
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
