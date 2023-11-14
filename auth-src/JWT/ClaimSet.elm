module JWT.ClaimSet exposing (ClaimSet, VerificationError(..), VerifyOptions, decoder, encoder, isValid)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional)
import Json.Encode as Encode
import Time exposing (Posix)


type alias ClaimSet =
    { iss : Maybe String
    , sub : Maybe String
    , aud : Maybe String
    , exp : Maybe Int
    , nbf : Maybe Int
    , iat : Maybe Int
    , jti : Maybe String
    , metadata : Dict String Decode.Value
    }


decoder : Decode.Decoder ClaimSet
decoder =
    Decode.succeed ClaimSet
        |> optional "iss" (Decode.maybe Decode.string) Nothing
        |> optional "sub" (Decode.maybe Decode.string) Nothing
        |> optional "aud" (Decode.maybe Decode.string) Nothing
        |> optional "exp" (Decode.maybe Decode.int) Nothing
        |> optional "nbf" (Decode.maybe Decode.int) Nothing
        |> optional "iat" (Decode.maybe Decode.int) Nothing
        |> optional "jti" (Decode.maybe Decode.string) Nothing
        |> custom (Decode.dict Decode.value)


encoder : ClaimSet -> Encode.Value
encoder claims =
    let
        metadata =
            Dict.toList claims.metadata
                |> List.map Just
    in
    metadata
        ++ [ claims.iss |> Maybe.map (\f -> ( "iss", Encode.string f ))
           , claims.sub |> Maybe.map (\f -> ( "sub", Encode.string f ))
           , claims.aud |> Maybe.map (\f -> ( "aud", Encode.string f ))
           , claims.exp |> Maybe.map (\f -> ( "exp", Encode.int f ))
           , claims.nbf |> Maybe.map (\f -> ( "nbf", Encode.int f ))
           , claims.iat |> Maybe.map (\f -> ( "iat", Encode.int f ))
           , claims.jti |> Maybe.map (\f -> ( "jti", Encode.string f ))
           ]
        |> List.filterMap identity
        |> Encode.object


type VerificationError
    = InvalidIssuer
    | InvalidAudience
    | InvalidSubject
    | InvalidJWTID
    | Expired
    | NotYetValid
    | NotYetIssued


type alias VerifyOptions =
    { issuer : Maybe String
    , audience : Maybe String
    , subject : Maybe String
    , jwtID : Maybe String
    , leeway : Int
    }


isValid : VerifyOptions -> Posix -> ClaimSet -> Result VerificationError Bool
isValid options now claims =
    checkIssuer claims.iss options.issuer
        |> Result.andThen
            (\_ -> checkAudience claims.aud options.audience)
        |> Result.andThen
            (\_ -> checkSubject claims.sub options.subject)
        |> Result.andThen
            (\_ -> checkID claims.jti options.jwtID)
        |> Result.andThen
            (\_ -> checkExpiration now options.leeway claims.exp)
        |> Result.andThen
            (\_ -> checkNotBefore now options.leeway claims.nbf)
        |> Result.andThen
            (\_ -> checkIssuedAt now options.leeway claims.iat)


checkIssuer : Maybe String -> Maybe String -> Result VerificationError Bool
checkIssuer claim option =
    case option of
        Nothing ->
            Ok True

        issuer ->
            if issuer == claim then
                Ok True

            else
                Err InvalidIssuer


checkAudience : Maybe String -> Maybe String -> Result VerificationError Bool
checkAudience claim option =
    case option of
        Nothing ->
            Ok True

        audience ->
            if audience == claim then
                Ok True

            else
                Err InvalidAudience


checkSubject : Maybe String -> Maybe String -> Result VerificationError Bool
checkSubject claim option =
    case option of
        Nothing ->
            Ok True

        subject ->
            if subject == claim then
                Ok True

            else
                Err InvalidSubject


checkID : Maybe String -> Maybe String -> Result VerificationError Bool
checkID claim option =
    case option of
        Nothing ->
            Ok True

        jwtID ->
            if jwtID == claim then
                Ok True

            else
                Err InvalidJWTID


checkExpiration : Posix -> Int -> Maybe Int -> Result VerificationError Bool
checkExpiration now leeway claim =
    case claim of
        Nothing ->
            Ok True

        Just expiration ->
            if Time.posixToMillis now - leeway < expiration * 1000 then
                Ok True

            else
                Err Expired


checkNotBefore : Posix -> Int -> Maybe Int -> Result VerificationError Bool
checkNotBefore now leeway claim =
    case claim of
        Nothing ->
            Ok True

        Just nbf ->
            if Time.posixToMillis now + leeway > nbf * 1000 then
                Ok True

            else
                Err NotYetValid


checkIssuedAt : Posix -> Int -> Maybe Int -> Result VerificationError Bool
checkIssuedAt now leeway claim =
    case claim of
        Nothing ->
            Ok True

        Just iat ->
            if Time.posixToMillis now + leeway > iat * 1000 then
                Ok True

            else
                Err NotYetIssued
