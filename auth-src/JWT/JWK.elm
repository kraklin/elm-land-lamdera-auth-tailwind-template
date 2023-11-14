module JWT.JWK exposing (JWK, decoder, encoder)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


type alias JWK =
    { kty : String
    , use : Maybe String
    , key_ops : Maybe (List String)
    , alg : Maybe String
    , kid : Maybe String
    , x5u : Maybe String
    , x5c : Maybe (List String)
    , x5t : Maybe String
    , x5t_S256 : Maybe String
    }


decoder : Decode.Decoder JWK
decoder =
    Decode.succeed JWK
        |> required "kty" Decode.string
        |> optional "use" (Decode.maybe Decode.string) Nothing
        |> optional "key_ops" (Decode.maybe <| Decode.list Decode.string) Nothing
        |> optional "alg" (Decode.maybe Decode.string) Nothing
        |> optional "kid" (Decode.maybe Decode.string) Nothing
        |> optional "x5u" (Decode.maybe Decode.string) Nothing
        |> optional "x5c" (Decode.maybe <| Decode.list Decode.string) Nothing
        |> optional "x5t" (Decode.maybe Decode.string) Nothing
        |> optional "x5t#S256" (Decode.maybe Decode.string) Nothing


encoder : JWK -> Encode.Value
encoder jwk =
    [ jwk.kty |> (\f -> Just ( "kty", Encode.string f ))
    , jwk.use |> Maybe.map (\f -> ( "use", Encode.string f ))
    , jwk.key_ops |> Maybe.map (\f -> ( "key_ops", Encode.list Encode.string f ))
    , jwk.alg |> Maybe.map (\f -> ( "alg", Encode.string f ))
    , jwk.kid |> Maybe.map (\f -> ( "kid", Encode.string f ))
    , jwk.x5u |> Maybe.map (\f -> ( "x5u", Encode.string f ))
    , jwk.x5c |> Maybe.map (\f -> ( "x5c", Encode.list Encode.string f ))
    , jwk.x5t |> Maybe.map (\f -> ( "x5t", Encode.string f ))
    , jwk.x5t_S256 |> Maybe.map (\f -> ( "x5t#S256", Encode.string f ))
    ]
        |> List.filterMap identity
        |> Encode.object
