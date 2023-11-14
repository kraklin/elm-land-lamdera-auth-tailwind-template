module JWT.JWKSet exposing (JWKSet, fromString, jwkSetDecoder)

import JWT.JWK as JWK exposing (JWK)
import Json.Decode as Decode


type alias JWKSet =
    { keys : List JWK }


fromString : String -> Result Decode.Error JWKSet
fromString string =
    Decode.decodeString jwkSetDecoder string


jwkSetDecoder : Decode.Decoder JWKSet
jwkSetDecoder =
    Decode.list JWK.decoder
        |> Decode.field "keys"
        |> Decode.map JWKSet
