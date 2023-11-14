module JWT exposing
    ( JWT(..), DecodeError(..), fromString
    , VerificationError(..), isValid, validate
    )

{-|


# JWT

@docs JWT, DecodeError, fromString


# Verification

@docs VerificationError, isValid, validate

-}

import JWT.ClaimSet exposing (VerifyOptions)
import JWT.JWS as JWS
import Task exposing (Task)
import Time exposing (Posix)


{-| A JSON Web Token.

Can be either a JWS (signed) or a JWE (encrypted). The latter is not yet implemented.

-}
type JWT
    = JWS JWS.JWS


{-| A structured error describing exactly how the decoder failed.
-}
type DecodeError
    = TokenTypeUnknown
    | JWSError JWS.DecodeError


{-| Decode a JWT from string.

    fromString "eyJhbGciOi..." == Ok ...
    fromString "" == Err ...
    fromString "definitelyNotAJWT" == Err ...

-}
fromString : String -> Result DecodeError JWT
fromString string =
    case String.split "." string of
        [ header, claims, signature ] ->
            JWS.fromParts header claims signature
                |> Result.mapError JWSError
                |> Result.map JWS

        -- TODO [ header_, encryptedKey, iv, ciphertext, authenticationTag ]
        _ ->
            Err TokenTypeUnknown


{-| A structured error describing all verification errors.
-}
type VerificationError
    = JWSVerificationError JWS.VerificationError


{-| Check if the token is valid.
-}
isValid : VerifyOptions -> String -> Posix -> JWT -> Result VerificationError Bool
isValid options key now token =
    case token of
        JWS token_ ->
            JWS.isValid options key now token_
                |> Result.mapError JWSVerificationError


{-| A task to check if the token is valid.
-}
validate : VerifyOptions -> String -> JWT -> Task Never (Result VerificationError Bool)
validate options key token =
    Time.now
        |> Task.andThen ((\now -> isValid options key now token) >> Task.succeed)
