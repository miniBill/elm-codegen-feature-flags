module CodeGen.FeatureFlags exposing
    ( bool, maybeString
    , fromFlags, withJsonConverters, withUrlConverters, withQueryKeyFormatter, generate
    )

{-|


# Flags

@docs bool, maybeString


# Builder methods

@docs fromFlags, withJsonConverters, withUrlConverters, withQueryKeyFormatter, generate

-}

import Elm
import Elm.Annotation as Type exposing (Annotation)
import Elm.Arg as Arg
import Elm.Case as Case
import Elm.Let as Let
import Elm.Op as Op
import Gen.Basics
import Gen.Dict
import Gen.Json.Decode as GJD
import Gen.Json.Decode.Pipeline as GPipeline
import Gen.Json.Encode as GJE
import Gen.List
import Gen.Maybe
import Gen.Tuple
import Gen.Url.Builder
import Gen.Url.Parser
import Gen.Url.Parser.Query as GUPQ


toSetterName : String -> String
toSetterName string =
    "set"
        ++ (String.uncons string
                |> Maybe.map (Tuple.mapFirst Char.toUpper >> (\( c, s ) -> String.cons c s))
                |> Maybe.withDefault string
           )


type Flag
    = F FlagDetails


type alias FlagDetails =
    { name : String

    -- when a flag is on its default value (`False` for `Bool` and `Nothing` for `Maybe String`)
    -- then it won't be serialized to json or query strings. this is to maintain "secrecy" (and
    -- short urls!) for unused flags.
    , default : Elm.Expression
    , decoder : Elm.Expression
    , encode : Elm.Expression -> Elm.Expression
    , toQuery : String -> Elm.Expression -> Elm.Expression
    , parser : String -> Elm.Expression
    , type_ : Annotation
    , prioritize : Elm.Expression -> Elm.Expression -> Elm.Expression
    }


unwrap : Flag -> FlagDetails
unwrap (F f) =
    f


{-| Create a feature flag for optional strings, with a default value of Nothing. The string
parameter here will be your flag name in the generated code, so always provide camelCase.

    fromFlags [ maybeString "alternateApiUrl" ]
        |> generate "FeatureFlags"
    -- produces a feature flag record definition of
    -- { alternateApiUrl : Maybe String }

-}
maybeString : String -> Flag
maybeString name =
    let
        type_ =
            Type.maybe Type.string
    in
    F
        { name = name
        , default = Elm.nothing
        , decoder = GJD.maybe GJD.string
        , encode = Gen.Maybe.map GJE.call_.string
        , parser = GUPQ.string
        , toQuery =
            \flagName val ->
                Gen.Maybe.map (Gen.Url.Builder.call_.string (Elm.string flagName)) val
        , type_ = type_
        , prioritize =
            \primary secondary ->
                Case.maybe primary
                    { nothing = secondary
                    , just =
                        ( "_"
                        , always primary
                        )
                    }
        }


{-| Create a feature flag for booleans, with a default value of False. The string parameter here
will be your flag name in the generated code, so always provide camelCase.

    fromFlags
        [ bool "analyticsV2"
        , bool "showPricingExplainer"
        ]
        |> generate "FeatureFlags"

    -- produces a feature flag record definition of
    -- { analyticsV2 : Bool
    -- , showPricingExplainer : Bool
    -- }

-}
bool : String -> Flag
bool name =
    let
        type_ =
            Type.bool

        whenTrue : Elm.Expression -> Elm.Expression -> Elm.Expression
        whenTrue output condition =
            Elm.ifThen condition (Elm.just output) Elm.nothing
    in
    F
        { name = name
        , default = Elm.bool False
        , decoder = GJD.bool
        , encode = whenTrue (GJE.bool True)
        , parser =
            \param ->
                GUPQ.map (Gen.Maybe.withDefault (Elm.bool False)) <|
                    GUPQ.enum param
                        (Gen.Dict.fromList
                            [ Elm.tuple (Elm.string "true") (Elm.bool True)
                            ]
                        )
        , toQuery =
            \flagName -> whenTrue (Gen.Url.Builder.string flagName "true")
        , type_ = type_
        , prioritize = Op.or
        }


{-| Start a builder for your feature flag module.

    fromFlags
        [ bool "snazzUpTheLoginButton"
        , maybeString "alternatePricingExplainer"
        ]

-}
fromFlags : List Flag -> Config
fromFlags flags =
    Config
        { flags = flags
        , includeJsonConverters = False
        , includeUrlConverters = False
        , queryKeyFormatter = identity
        }


{-| Bundle in a JSON decoder and an encoder. Introduces dependencies on elm/json and
NoRedInk/elm-json-decode-pipeline. All fields will be optional in decoding, and default values will
be omitted on serialization.

    fromFlags
        [ bool "loadSlowly"
        , bool "repeatedlyAskForNotificationPermissions"
        ]
        |> withJsonConverters
        |> generate "Flags"

    -- Flags will now expose:
    --   - `decoder : Json.Decode.Decoder FeatureFlags`
    --   - `encode : FeatureFlags -> Json.Encode.Value`
    Json.Decode.decodeString
        Flags.decoder
        "{\"repeatedlyAskForNotificationPermissions\":true}"
        == Ok
            { loadSlowly = False
            , repeatedlyAskForNotificationPermissions = True
            }

    Json.Encode.encode 0
        (Flags.encode
            { loadSlowly = True
            , repeatedlyAskForNotificationPermissions = False
            }
        )
        == "{\"loadSlowly\":true}"

-}
withJsonConverters : Config -> Config
withJsonConverters (Config config) =
    Config { config | includeJsonConverters = True }


{-| Bundle in a URL querystring parser and a builder. Introduces a dependency on elm/url.
Querystring keys match the fieldname by default. All fields will be optional in decoding, and
default values will be omitted on serialization.

    fromFlags [ maybeString "betterNameForUser" ]
        |> withUrlConverters
        |> generate "Flags"

    -- Flags will now expose:
    --   - `fromUrl : Url.Url -> FeatureFlags`
    --   - `toParams : FeatureFlags -> List Url.Builder.QueryParameter`
    Maybe.map Flags.fromUrl
        Url.fromString
        "https://trustworthy.com?betterNameForUser=James"
        == Just { betterNameForUser = Just "James" }

    Url.Builder.crossOrigin
        "https://trustworthy.com"
        []
        (Url.Flags.toParams { betterNameForUser = Nothing })
        == "https://trustworthy.com/"

-}
withUrlConverters : Config -> Config
withUrlConverters (Config config) =
    Config { config | includeUrlConverters = True }


{-| Apply your own logic for creating URL query keys. Takes the fieldname as an input. Ignored if
you don't call `withUrlConverters`.

    fromFlags [ bool "refreshAtRandom" ]
        |> withUrlConverters
        |> withQueryKeyFormatter (\fieldName -> "f-" ++ fieldName)
        |> generate "Flags"

    Maybe.map Flags.fromUrl
        (Url.fromString "https://trustworthy.com?f-refreshAtRandom=true")
        == Just { refreshAtRandom = True }

    Url.Builder.crossOrigin
        "https://trustworthy.com"
        []
        (Url.Flags.toParams { refreshAtRandom = True })
        == "https://trustworthy.com/?f-refreshAtRandom=true"

-}
withQueryKeyFormatter : (String -> String) -> Config -> Config
withQueryKeyFormatter f (Config config) =
    Config { config | queryKeyFormatter = f }


type Config
    = Config
        { flags : List Flag
        , includeJsonConverters : Bool
        , includeUrlConverters : Bool
        , queryKeyFormatter : String -> String
        }


{-| Generate a file based on the config you've built. elm-codegen will name the module whatever you
provide here.

    fromFlags
        [ bool "featureA"
        , maybeString "featureB"
        ]
        |> withJsonConverters
        |> withUrlConverters
        |> withQueryKeyFormatter (\fieldName -> "f-" ++ fieldName)
        |> generate "Flags"

-}
generate : String -> Config -> Elm.File
generate fileName (Config config) =
    let
        flags =
            List.map unwrap config.flags

        featureFlagsType =
            Type.named [] "FeatureFlags"

        whenAppliedFunc :
            { name : String, doc : String }
            -> (Elm.Expression -> Elm.Expression -> Elm.Expression)
            -> Elm.Declaration
        whenAppliedFunc textStuff =
            exposedDeclaration textStuff
                << Elm.withType
                    (Type.function
                        [ Type.namedWith [] "WhenApplied" [ Type.var "x" ], featureFlagsType ]
                        (Type.list (Type.var "x"))
                    )
                << Elm.fn2
                    (Arg.var "applied")
                    (Arg.var "featureFlags")

        exposedDeclaration : { name : String, doc : String } -> Elm.Expression -> Elm.Declaration
        exposedDeclaration { name, doc } =
            Elm.expose
                << Elm.withDocumentation doc
                << Elm.declaration name
    in
    Elm.file [ fileName ] <|
        [ Elm.group
            [ -- Elm.docs """
              -- # Feature Flags!
              -- @docs FeatureFlags, default
              -- # De/Serialization
              -- @docs decoder, encode, fromUrl, toParams
              -- # Iterating over flags/meta-usage
              -- @docs WhenApplied, applyToActive, applyToAll
              -- """
              --   ,
              Elm.expose <|
                Elm.withDocumentation "Your generated FeatureFlags type. Has a field for each flag in your codegen config." <|
                    Elm.alias "FeatureFlags" <|
                        Type.record
                            (List.map (\flag -> ( flag.name, flag.type_ )) flags)
            , exposedDeclaration
                { name = "default"
                , doc = "An instance of FeatureFlags with all default values."
                }
              <|
                Elm.withType featureFlagsType <|
                    Elm.record
                        (List.map (\flag -> ( flag.name, flag.default )) flags)
            ]
        , if config.includeJsonConverters then
            Elm.group
                [ exposedDeclaration
                    { name = "decoder"
                    , doc = "A JSON decoder where all fields are optional. Will succeed no matter what!"
                    }
                  <|
                    Elm.withType (Type.namedWith [ "Json", "Decode" ] "Decoder" [ featureFlagsType ]) <|
                        List.foldl
                            (\flag decoder ->
                                decoder
                                    |> Op.pipe
                                        (Elm.apply GPipeline.values_.optional
                                            [ Elm.string flag.name
                                            , flag.decoder
                                            , flag.default
                                            ]
                                        )
                            )
                            (GJD.succeed (Elm.val "FeatureFlags"))
                            flags
                , exposedDeclaration
                    { name = "encode"
                    , doc = "Encodes FeatureFlags to JSON."
                    }
                  <|
                    Elm.withType (Type.function [ featureFlagsType ] (Type.named [ "Json", "Encode" ] "Value")) <|
                        Elm.fn
                            (Arg.var "featureFlags")
                            (\featureFlags ->
                                GJE.call_.object
                                    (Gen.List.filterMap Gen.Basics.identity
                                        (List.map
                                            (\flag ->
                                                Gen.Maybe.map
                                                    (Gen.Tuple.pair (Elm.string flag.name))
                                                    (flag.encode (featureFlags |> Elm.get flag.name))
                                            )
                                            flags
                                        )
                                    )
                            )
                ]

          else
            Elm.group []
        , if config.includeUrlConverters then
            Elm.group
                [ exposedDeclaration
                    { name = "fromUrl"
                    , doc = "Decodes from a URL querystring. Ignores the path. (Maybe we should expose a Url.Query.Parser so you can be more precise if you like?)"
                    }
                  <|
                    Elm.withType (Type.function [ Type.named [ "Url" ] "Url" ] featureFlagsType) <|
                        Elm.fn
                            (Arg.varWith "url" (Type.named [ "Url" ] "Url"))
                            (\url ->
                                Let.letIn
                                    (\droppedPath ->
                                        Elm.apply (Elm.val "FeatureFlags")
                                            (List.map
                                                (\flag ->
                                                    Gen.Maybe.withDefault flag.default
                                                        (Gen.Url.Parser.parse
                                                            (Gen.Url.Parser.query
                                                                (flag.parser (config.queryKeyFormatter flag.name))
                                                            )
                                                            droppedPath
                                                        )
                                                )
                                                flags
                                            )
                                    )
                                    |> Let.value "droppedPath"
                                        (url |> Elm.updateRecord [ ( "path", Elm.string "" ) ])
                                    |> Let.toExpression
                            )
                , exposedDeclaration
                    { name = "toParams"
                    , doc = "Querystring builder."
                    }
                  <|
                    Elm.withType
                        (Type.function [ featureFlagsType ]
                            (Type.list (Type.named [ "Url", "Builder" ] "QueryParameter"))
                        )
                    <|
                        Elm.fn
                            (Arg.var "featureFlags")
                            (\featureFlags ->
                                Gen.List.filterMap Gen.Basics.identity
                                    (List.map
                                        (\flag ->
                                            flag.toQuery
                                                (config.queryKeyFormatter flag.name)
                                                (featureFlags |> Elm.get flag.name)
                                        )
                                        flags
                                    )
                            )
                ]

          else
            Elm.group []
        , exposedDeclaration
            { name = "or"
            , doc = """
Join two FeatureFlags together, taking each first value if non-default, else the second. Most useful
when reconciling feature flags parsed from two sources (e.g. URL querystrings and JSON).

    -- example type is { a: Maybe String, b: Bool }
    or { a = Just "a", b = False } { a = Just "aa", b = True }
        == { a = Just "a", b = True }
"""
            }
          <|
            Elm.withType (Type.function [ featureFlagsType, featureFlagsType ] featureFlagsType) <|
                Elm.fn2
                    (Arg.var "primary")
                    (Arg.var "secondary")
                    (\primary secondary ->
                        Elm.record
                            (List.map
                                (\flag ->
                                    ( flag.name
                                    , flag.prioritize (primary |> Elm.get flag.name) (secondary |> Elm.get flag.name)
                                    )
                                )
                                flags
                            )
                    )
        , Elm.group
            [ Elm.expose <|
                Elm.withDocumentation """
A meta-type of FeatureFlag, where each field (same name) has a type of "a function going from
`yourtype` to `x`". Useful for things like making a record of msg handlers for every feature flag,
or view functions. Best used in conjunction with [`applyToActive`](#applyToActive) or
[`applyToAll`](#applyToAll).
""" <|
                    Elm.aliasWith "WhenApplied" [ "x" ] <|
                        Type.record
                            (List.map (\flag -> ( flag.name, Type.function [ flag.type_ ] (Type.var "x") )) flags)

            -- TODO: with both of these application funcs, you could actually use the real input
            -- type people give, if you're feeling creative enough
            , whenAppliedFunc
                { name = "applyToActive"
                , doc = """
Invoke a callback for each flag that has a non-default value.

    -- example type is { a: Bool, b: Bool }
    applyToActive
        { a = \\_ -> "Feat A", b = \\_ -> "Feat B" }
        { a = True, b = False }
        == [ "Feat A" ]
"""
                }
              <|
                \applied featureFlags ->
                    Gen.List.filterMap Gen.Basics.identity
                        (List.map
                            (\flag ->
                                Elm.ifThen
                                    (Op.notEqual (featureFlags |> Elm.get flag.name)
                                        (Elm.val "default" |> Elm.get flag.name)
                                    )
                                    (Elm.just
                                        (Elm.apply
                                            (applied |> Elm.get flag.name)
                                            [ featureFlags |> Elm.get flag.name ]
                                        )
                                    )
                                    Elm.nothing
                            )
                            flags
                        )
            , -- TODO: what the hell is with the type annotation here
              whenAppliedFunc { name = "applyToAll", doc = """
Invoke a callback for each flag, regardless of status. (Ignore the type variable name there, it's a generation artifact.)

    -- example type is { a: Maybe String, b: Maybe String, c: Bool }
    applyToAll
        { a = \\x -> "A: " ++ Maybe.withDefault "<unset>" x
        , b = \\x -> "B: " ++ Maybe.withDefault "<unset>" x
        , c = \\x -> "C: " ++ (if x then "active" else "inactive")
        }
        { a = Just "content", b = Nothing, c = False }
        == [ "A: content", "B: <unset>", "C: inactive" ]
""" } <|
                \applied featureFlags ->
                    Elm.list
                        (List.map
                            (\flag ->
                                Elm.apply
                                    (applied |> Elm.get flag.name)
                                    [ featureFlags |> Elm.get flag.name ]
                            )
                            flags
                        )
            ]
        , Elm.group <|
            List.map
                (\flag ->
                    exposedDeclaration
                        { name = toSetterName flag.name
                        , doc = "Simple setter function for the `" ++ flag.name ++ "` flag."
                        }
                    <|
                        Elm.withType (Type.function [ flag.type_, featureFlagsType ] featureFlagsType) <|
                            Elm.fn2
                                -- agonizing over the argument order here
                                (Arg.var flag.name)
                                (Arg.var "featureFlags")
                                (\flagVal featureFlags ->
                                    Elm.updateRecord [ ( flag.name, flagVal ) ] featureFlags
                                )
                )
                flags
        ]
