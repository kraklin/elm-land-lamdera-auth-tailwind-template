module Bridge exposing (..)

import Auth.Common


type ToBackend
    = SmashedLikeButton
    | AuthToBackend Auth.Common.ToBackend
    | LogOut
    | GetUser
