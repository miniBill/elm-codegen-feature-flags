module Example exposing (..)

import Expect exposing (Expectation)
import FeatureFlags exposing (FeatureFlags)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (..)
import Url
import Url.Builder


fullCustom : FeatureFlags
fullCustom =
    { customApiDomain = Just "https://evil.com"
    , eagerLoadInvites = True
    , largeLoginButton = True
    , largeSignupButton = True
    , useExperimentalAnimationLibrary = True
    , welcomeMessage = "Who goes there?"
    }


mixedCustomAndDefault : FeatureFlags
mixedCustomAndDefault =
    { customApiDomain = Just "https://evil.com"
    , eagerLoadInvites = True
    , largeLoginButton = False
    , largeSignupButton = True
    , useExperimentalAnimationLibrary = False
    , welcomeMessage = "Hello :)"
    }


differentMixedCustomAndDefault : FeatureFlags
differentMixedCustomAndDefault =
    { customApiDomain = Nothing
    , eagerLoadInvites = True
    , largeLoginButton = True
    , largeSignupButton = False
    , useExperimentalAnimationLibrary = False
    , welcomeMessage = "Who goes there?"
    }


happySad : Bool -> String
happySad x =
    if x then
        ":)"

    else
        ":("


suite : Test
suite =
    describe "feature flags generation"
        [ test "defaults are reasonable or configurable" <|
            \_ ->
                Expect.equal
                    { customApiDomain = Nothing
                    , eagerLoadInvites = False
                    , largeLoginButton = False
                    , largeSignupButton = False
                    , useExperimentalAnimationLibrary = False
                    , welcomeMessage = "Hello :)"
                    }
                    FeatureFlags.default
        , describe "json encoding/decoding" <|
            [ test "is isomorphic for defaults" <|
                \_ ->
                    Expect.equal (Ok FeatureFlags.default)
                        (JD.decodeValue FeatureFlags.decoder
                            (FeatureFlags.encode FeatureFlags.default)
                        )
            , test "is isomorphic for custom values" <|
                \_ ->
                    Expect.equal (Ok fullCustom)
                        (JD.decodeValue FeatureFlags.decoder
                            (FeatureFlags.encode fullCustom)
                        )
            , test "hides default values when serialized" <|
                \_ ->
                    JE.encode 0 (FeatureFlags.encode FeatureFlags.default)
                        |> Expect.equal "{}"
            ]
        , describe "url encoding/decoding" <|
            [ test "is isomorphic for defaults" <|
                \_ ->
                    FeatureFlags.toParams FeatureFlags.default
                        |> Url.Builder.crossOrigin "https://good.com" []
                        |> Url.fromString
                        |> Maybe.map FeatureFlags.fromUrl
                        |> Expect.equal (Just FeatureFlags.default)
            , test "is isomorphic for custom values" <|
                \_ ->
                    FeatureFlags.toParams fullCustom
                        |> Url.Builder.crossOrigin "https://good.com" []
                        |> Url.fromString
                        |> Maybe.map FeatureFlags.fromUrl
                        |> Expect.equal (Just fullCustom)
            , test "hides default values when serialized" <|
                \_ ->
                    FeatureFlags.toParams FeatureFlags.default
                        |> Expect.equal []
            ]
        , describe "using WhenApplied" <|
            let
                basicWhenApplied : FeatureFlags.WhenApplied String
                basicWhenApplied =
                    { customApiDomain = Maybe.withDefault "https://oops.com"
                    , eagerLoadInvites = happySad
                    , largeLoginButton = happySad
                    , largeSignupButton = happySad
                    , useExperimentalAnimationLibrary = happySad
                    , welcomeMessage = identity
                    }
            in
            [ test "applyToActive produces an empty list on default flags" <|
                \_ ->
                    FeatureFlags.applyToActive basicWhenApplied FeatureFlags.default
                        |> Expect.equal []
            , test "applyToActive produces a full list on custom flags" <|
                \_ ->
                    FeatureFlags.applyToActive basicWhenApplied fullCustom
                        |> Expect.equal
                            [ "https://evil.com"
                            , ":)"
                            , ":)"
                            , ":)"
                            , ":)"
                            , "Who goes there?"
                            ]
            , test "applyToActive preserves field order given at codegen time" <|
                \_ ->
                    FeatureFlags.applyToActive basicWhenApplied mixedCustomAndDefault
                        |> Expect.equal
                            [ "https://evil.com"
                            , ":)"
                            , ":)"
                            ]
            , test "applyToAll produces a full list even on default flags" <|
                \_ ->
                    FeatureFlags.applyToAll basicWhenApplied FeatureFlags.default
                        |> Expect.equal
                            [ "https://oops.com"
                            , ":("
                            , ":("
                            , ":("
                            , ":("
                            , "Hello :)"
                            ]
            , test "applyToAll preserves field order given at codegen time" <|
                \_ ->
                    FeatureFlags.applyToAll basicWhenApplied mixedCustomAndDefault
                        |> Expect.equal
                            [ "https://evil.com"
                            , ":)"
                            , ":("
                            , ":)"
                            , ":("
                            , "Hello :)"
                            ]
            ]
        , describe "using `or`"
            [ test "default values from the first record will fall back on to the second record" <|
                \_ ->
                    Expect.all
                        [ \mixed ->
                            FeatureFlags.or mixed fullCustom
                                |> Expect.equal fullCustom
                        , \mixed ->
                            FeatureFlags.or mixed differentMixedCustomAndDefault
                                |> Expect.equal
                                    { customApiDomain = Just "https://evil.com"
                                    , eagerLoadInvites = True
                                    , largeLoginButton = True
                                    , largeSignupButton = True
                                    , useExperimentalAnimationLibrary = False
                                    , welcomeMessage = "Who goes there?"
                                    }
                        ]
                        mixedCustomAndDefault
            , test "or-ing two non-default values will prefer the first" <|
                \_ ->
                    let
                        d =
                            FeatureFlags.default
                    in
                    FeatureFlags.or
                        { d | customApiDomain = Just "https://fame.com", welcomeMessage = "one" }
                        { d | customApiDomain = Just "https://fortune.com", welcomeMessage = "two" }
                        |> Expect.equal
                            { d | customApiDomain = Just "https://fame.com", welcomeMessage = "one" }
            , test "or-ing a default is a no-op" <|
                \_ ->
                    Expect.all
                        [ \default ->
                            FeatureFlags.or mixedCustomAndDefault default
                                |> Expect.equal mixedCustomAndDefault
                        , \default ->
                            FeatureFlags.or fullCustom default
                                |> Expect.equal fullCustom
                        , \default ->
                            FeatureFlags.or default mixedCustomAndDefault
                                |> Expect.equal mixedCustomAndDefault
                        , \default ->
                            FeatureFlags.or default fullCustom
                                |> Expect.equal fullCustom
                        ]
                        FeatureFlags.default
            ]
        ]
