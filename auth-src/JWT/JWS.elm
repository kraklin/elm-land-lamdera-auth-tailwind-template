module JWT.JWS exposing (DecodeError(..), Header, JWS, VerificationError(..), decode, fromParts, isValid)

import Base64.Decode as B64Decode
import Base64.Encode as B64Encode
import Bytes exposing (Bytes)
import Bytes.Decode
import Crypto.HMAC
import JWT.ClaimSet as ClaimSet exposing (VerifyOptions)
import JWT.JWK as JWK
import JWT.UrlBase64 as UrlBase64
import Json.Decode as JDecode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as JEncode
import Result exposing (andThen, map, mapError)
import Time exposing (Posix)
import Word.Bytes


type alias JWS =
    { signature : List Int
    , header : Header
    , claims : ClaimSet.ClaimSet
    }


type alias Header =
    { alg : String
    , jku : Maybe String
    , jwk : Maybe JWK.JWK
    , kid : Maybe String
    , x5u : Maybe String
    , x5c : Maybe (List String)
    , x5t : Maybe String
    , x5t_S256 : Maybe String
    , typ : Maybe String
    , cty : Maybe String
    , crit : Maybe (List String)
    }


type DecodeError
    = Base64DecodeError
    | MalformedSignature
    | InvalidHeader JDecode.Error
    | InvalidClaims JDecode.Error


fromParts : String -> String -> String -> Result DecodeError JWS
fromParts header claims signature =
    let
        decode_ d part =
            UrlBase64.decode (B64Decode.decode d) part

        bytesDecoder len =
            Bytes.Decode.loop ( len, [] ) <|
                \( n, xs ) ->
                    if n <= 0 then
                        Bytes.Decode.succeed (Bytes.Decode.Done xs)

                    else
                        Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, x :: xs )) Bytes.Decode.unsignedInt8

        decodeBytes bytes =
            Bytes.Decode.decode (bytesDecoder (Bytes.width bytes)) bytes
                |> Maybe.map List.reverse
    in
    case
        ( decode_ B64Decode.string header
        , decode_ B64Decode.string claims
        , decode_ B64Decode.bytes signature
        )
    of
        ( Ok header_, Ok claims_, Ok signature_ ) ->
            case decodeBytes signature_ of
                Just sig ->
                    decode header_ claims_ sig

                Nothing ->
                    Err MalformedSignature

        _ ->
            Err Base64DecodeError


decode : String -> String -> List Int -> Result DecodeError JWS
decode header claims signature =
    JDecode.decodeString headerDecoder header
        |> mapError InvalidHeader
        |> andThen
            (\header_ ->
                JDecode.decodeString ClaimSet.decoder claims
                    |> mapError InvalidClaims
                    |> map (JWS signature header_)
            )


encodeParts : JWS -> List String
encodeParts token =
    [ JEncode.encode 0 <| headerEncoder token.header
    , JEncode.encode 0 <| ClaimSet.encoder token.claims
    ]


headerDecoder : JDecode.Decoder Header
headerDecoder =
    JDecode.succeed Header
        |> required "alg" JDecode.string
        |> optional "jku" (JDecode.maybe JDecode.string) Nothing
        |> optional "jwk" (JDecode.maybe JWK.decoder) Nothing
        |> optional "kid" (JDecode.maybe JDecode.string) Nothing
        |> optional "x5u" (JDecode.maybe JDecode.string) Nothing
        |> optional "x5c" (JDecode.maybe <| JDecode.list JDecode.string) Nothing
        |> optional "x5t" (JDecode.maybe JDecode.string) Nothing
        |> optional "x5t#S256" (JDecode.maybe JDecode.string) Nothing
        |> optional "typ" (JDecode.maybe JDecode.string) Nothing
        |> optional "cty" (JDecode.maybe JDecode.string) Nothing
        |> optional "crit" (JDecode.maybe <| JDecode.list JDecode.string) Nothing


headerEncoder : Header -> JEncode.Value
headerEncoder header =
    [ header.alg |> (\f -> Just ( "alg", JEncode.string f ))
    , header.jku |> Maybe.map (\f -> ( "jku", JEncode.string f ))
    , header.jwk |> Maybe.map (\f -> ( "jwk", JWK.encoder f ))
    , header.kid |> Maybe.map (\f -> ( "kid", JEncode.string f ))
    , header.x5u |> Maybe.map (\f -> ( "x5u", JEncode.string f ))
    , header.x5c |> Maybe.map (\f -> ( "x5c", JEncode.list JEncode.string f ))
    , header.x5t |> Maybe.map (\f -> ( "x5t", JEncode.string f ))
    , header.x5t_S256 |> Maybe.map (\f -> ( "x5t#S256", JEncode.string f ))
    , header.typ |> Maybe.map (\f -> ( "typ", JEncode.string f ))
    , header.cty |> Maybe.map (\f -> ( "cty", JEncode.string f ))
    , header.crit |> Maybe.map (\f -> ( "crit", JEncode.list JEncode.string f ))
    ]
        |> List.filterMap identity
        |> JEncode.object


type VerificationError
    = UnsupportedAlgorithm
    | InvalidSignature
    | ClaimSet ClaimSet.VerificationError


isValid : VerifyOptions -> String -> Posix -> JWS -> Result VerificationError Bool
isValid options key now token =
    checkSignature key token
        |> Result.andThen
            (\_ ->
                ClaimSet.isValid options now token.claims
                    |> Result.mapError ClaimSet
            )


checkSignature : String -> JWS -> Result VerificationError Bool
checkSignature key token =
    let
        payload =
            encodeParts token
                |> List.map (\p -> UrlBase64.encode B64Encode.encode (B64Encode.string p))
                |> String.join "."
                |> Word.Bytes.fromUTF8

        detectAlg =
            case token.header.alg of
                "HS256" ->
                    Ok Crypto.HMAC.sha256

                _ ->
                    Err UnsupportedAlgorithm

        calculated alg =
            Crypto.HMAC.digestBytes alg (Word.Bytes.fromUTF8 key) payload
    in
    detectAlg
        |> Result.andThen
            (\alg ->
                if token.signature == calculated alg then
                    Ok True

                else
                    Err InvalidSignature
            )
