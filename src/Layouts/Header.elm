module Layouts.Header exposing (Model, Msg, Props, layout)

import Auth exposing (User)
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import View exposing (View)


type alias Props =
    { user : User }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update
        , view = view props
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { isMenuOpened : Bool
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { isMenuOpened = False }
    , Effect.none
    )



-- UPDATE


type Msg
    = LogOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        LogOut ->
            ( model
            , Effect.logout
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Props -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props { toContentMsg, model, content } =
    { title = content.title
    , body =
        [ {-
             This example requires updating your template:

             ```
             <html class="h-full">
             <body class="h-full">
             ```
          -}
          Html.div
            [ Attrs.class "min-h-full"
            ]
            [ Html.nav
                [ Attrs.class "border-b border-gray-200 bg-white"
                ]
                [ Html.div
                    [ Attrs.class "mx-auto max-w-7xl px-4 sm:px-6 lg:px-8"
                    ]
                    [ Html.div
                        [ Attrs.class "flex h-16 justify-between"
                        ]
                        [ Html.div
                            [ Attrs.class "flex"
                            ]
                            [ Html.div
                                [ Attrs.class "flex flex-shrink-0 items-center"
                                ]
                                [ Html.img
                                    [ Attrs.class "block h-8 w-auto lg:hidden"
                                    , Attrs.src "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600"
                                    , Attrs.alt "Your Company"
                                    ]
                                    []
                                , Html.img
                                    [ Attrs.class "hidden h-8 w-auto lg:block"
                                    , Attrs.src "https://tailwindui.com/img/logos/mark.svg?color=indigo&shade=600"
                                    , Attrs.alt "Your Company"
                                    ]
                                    []
                                ]
                            , Html.div
                                [ Attrs.class "hidden sm:-my-px sm:ml-6 sm:flex sm:space-x-8"
                                ]
                                [ {- Current: "border-indigo-500 text-gray-900", Default: "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700" -}
                                  Html.a
                                    [ Attrs.href "#"
                                    , Attrs.class "border-indigo-500 text-gray-900 inline-flex items-center border-b-2 px-1 pt-1 text-sm font-medium"
                                    , Attrs.attribute "aria-current" "page"
                                    ]
                                    [ Html.text "Dashboard" ]
                                , Html.a
                                    [ Attrs.href "#"
                                    , Attrs.class "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center border-b-2 px-1 pt-1 text-sm font-medium"
                                    ]
                                    [ Html.text "Team" ]
                                , Html.a
                                    [ Attrs.href "#"
                                    , Attrs.class "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center border-b-2 px-1 pt-1 text-sm font-medium"
                                    ]
                                    [ Html.text "Projects" ]
                                , Html.a
                                    [ Attrs.href "#"
                                    , Attrs.class "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center border-b-2 px-1 pt-1 text-sm font-medium"
                                    ]
                                    [ Html.text "Calendar" ]
                                ]
                            ]
                        , Html.div
                            [ Attrs.class "hidden sm:ml-6 sm:flex sm:items-center"
                            ]
                            [ Html.div
                                [ Attrs.class "relative ml-3"
                                ]
                                [ Html.div [ Attrs.class "items-center sm:flex sm:space-x-4" ]
                                    [ Html.img
                                        [ Attrs.class "h-8 w-8 rounded-full"
                                        , Attrs.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                        , Attrs.alt ""
                                        ]
                                        []
                                    , Html.span [] [ Html.text props.user.email ]
                                    , Html.button
                                        [ Attrs.class "px-4 py-2 rounded bg-gray-900 text-white font-bold"
                                        , Events.onClick <| toContentMsg LogOut
                                        ]
                                        [ Html.text "Log Out" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            , Html.main_ [ Attrs.class "py-10" ]
                [ Html.div
                    [ Attrs.class "mx-auto max-w-7xl sm:px-6 lg:px-8"
                    ]
                    content.body
                ]
            ]
        ]
    }
