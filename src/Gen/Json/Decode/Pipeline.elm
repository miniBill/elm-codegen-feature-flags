module Gen.Json.Decode.Pipeline exposing
    ( moduleName_, required, requiredAt, optional, optionalAt, hardcoded
    , custom, resolve, call_, values_
    )

{-|
# Generated bindings for Json.Decode.Pipeline

@docs moduleName_, required, requiredAt, optional, optionalAt, hardcoded
@docs custom, resolve, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Json", "Decode", "Pipeline" ]


{-| Decode a required field.

    import Json.Decode as Decode exposing (Decoder, int, string)
    import Json.Decode.Pipeline exposing (required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> required "name" string
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com", "name": "Sam"}
        """

    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }

required: 
    String
    -> Json.Decode.Decoder a
    -> Json.Decode.Decoder (a -> b)
    -> Json.Decode.Decoder b
-}
required : String -> Elm.Expression -> Elm.Expression -> Elm.Expression
required requiredArg_ requiredArg_0 requiredArg_1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "required"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "a" ]
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.function [ Type.var "a" ] (Type.var "b") ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "b" ]
                          )
                     )
             }
        )
        [ Elm.string requiredArg_, requiredArg_0, requiredArg_1 ]


{-| Decode a required nested field.

requiredAt: 
    List String
    -> Json.Decode.Decoder a
    -> Json.Decode.Decoder (a -> b)
    -> Json.Decode.Decoder b
-}
requiredAt : List String -> Elm.Expression -> Elm.Expression -> Elm.Expression
requiredAt requiredAtArg_ requiredAtArg_0 requiredAtArg_1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "requiredAt"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list Type.string
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "a" ]
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.function [ Type.var "a" ] (Type.var "b") ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "b" ]
                          )
                     )
             }
        )
        [ Elm.list (List.map Elm.string requiredAtArg_)
        , requiredAtArg_0
        , requiredAtArg_1
        ]


{-| Decode a field that may be missing or have a null value. If the field is
missing, then it decodes as the `fallback` value. If the field is present,
then `valDecoder` is used to decode its value. If `valDecoder` fails on a
`null` value, then the `fallback` is used as if the field were missing
entirely.

    import Json.Decode as Decode exposing (Decoder, int, null, oneOf, string)
    import Json.Decode.Pipeline exposing (optional, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> optional "name" string "blah"
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com" }
        """

    -- Ok { id = 123, name = "blah", email = "sam@example.com" }

Because `valDecoder` is given an opportunity to decode `null` values before
resorting to the `fallback`, you can distinguish between missing and `null`
values if you need to:

    userDecoder2 =
        Decode.succeed User
            |> required "id" int
            |> optional "name" (oneOf [ string, null "NULL" ]) "MISSING"
            |> required "email" string

optional: 
    String
    -> Json.Decode.Decoder a
    -> a
    -> Json.Decode.Decoder (a -> b)
    -> Json.Decode.Decoder b
-}
optional :
    String
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
optional optionalArg_ optionalArg_0 optionalArg_1 optionalArg_2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "optional"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "a" ]
                          , Type.var "a"
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.function [ Type.var "a" ] (Type.var "b") ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "b" ]
                          )
                     )
             }
        )
        [ Elm.string optionalArg_, optionalArg_0, optionalArg_1, optionalArg_2 ]


{-| Decode an optional nested field.

optionalAt: 
    List String
    -> Json.Decode.Decoder a
    -> a
    -> Json.Decode.Decoder (a -> b)
    -> Json.Decode.Decoder b
-}
optionalAt :
    List String
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
optionalAt optionalAtArg_ optionalAtArg_0 optionalAtArg_1 optionalAtArg_2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "optionalAt"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list Type.string
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "a" ]
                          , Type.var "a"
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.function [ Type.var "a" ] (Type.var "b") ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "b" ]
                          )
                     )
             }
        )
        [ Elm.list (List.map Elm.string optionalAtArg_)
        , optionalAtArg_0
        , optionalAtArg_1
        , optionalAtArg_2
        ]


{-| Rather than decoding anything, use a fixed value for the next step in the
pipeline. `harcoded` does not look at the JSON at all.

    import Json.Decode as Decode exposing (Decoder, int, string)
    import Json.Decode.Pipeline exposing (required)

    type alias User =
        { id : Int
        , email : String
        , followers : Int
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> required "email" string
            |> hardcoded 0

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com"}
        """

    -- Ok { id = 123, email = "sam@example.com", followers = 0 }

hardcoded: a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
-}
hardcoded : Elm.Expression -> Elm.Expression -> Elm.Expression
hardcoded hardcodedArg_ hardcodedArg_0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "hardcoded"
             , annotation =
                 Just
                     (Type.function
                          [ Type.var "a"
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.function [ Type.var "a" ] (Type.var "b") ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "b" ]
                          )
                     )
             }
        )
        [ hardcodedArg_, hardcodedArg_0 ]


{-| Run the given decoder and feed its result into the pipeline at this point.

Consider this example.

    import Json.Decode as Decode exposing (Decoder, at, int, string)
    import Json.Decode.Pipeline exposing (custom, required)

    type alias User =
        { id : Int
        , name : String
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        Decode.succeed User
            |> required "id" int
            |> custom (at [ "profile", "name" ] string)
            |> required "email" string

    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {
            "id": 123,
            "email": "sam@example.com",
            "profile": {"name": "Sam"}
          }
        """

    -- Ok { id = 123, name = "Sam", email = "sam@example.com" }

custom: Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
-}
custom : Elm.Expression -> Elm.Expression -> Elm.Expression
custom customArg_ customArg_0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "custom"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "a" ]
                          , Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.function [ Type.var "a" ] (Type.var "b") ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "b" ]
                          )
                     )
             }
        )
        [ customArg_, customArg_0 ]


{-| Convert a `Decoder (Result x a)` into a `Decoder a`. Useful when you want
to perform some custom processing just before completing the decoding operation.

    import Json.Decode as Decode exposing (Decoder, float, int, string)
    import Json.Decode.Pipeline exposing (required, resolve)

    type alias User =
        { id : Int
        , email : String
        }

    userDecoder : Decoder User
    userDecoder =
        let
            -- toDecoder gets run *after* all the
            -- (|> required ...) steps are done.
            toDecoder : Int -> String -> Int -> Decoder User
            toDecoder id email version =
                if version > 2 then
                    Decode.succeed (User id email)

                else
                    fail "This JSON is from a deprecated source. Please upgrade!"
        in
        Decode.succeed toDecoder
            |> required "id" int
            |> required "email" string
            |> required "version" int
            -- version is part of toDecoder,
            |> resolve

    -- but it is not a part of User
    result : Result String User
    result =
        Decode.decodeString
            userDecoder
            """
          {"id": 123, "email": "sam@example.com", "version": 1}
        """

    -- Err "This JSON is from a deprecated source. Please upgrade!"

resolve: Json.Decode.Decoder (Json.Decode.Decoder a) -> Json.Decode.Decoder a
-}
resolve : Elm.Expression -> Elm.Expression
resolve resolveArg_ =
    Elm.apply
        (Elm.value
             { importFrom = [ "Json", "Decode", "Pipeline" ]
             , name = "resolve"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "a" ]
                              ]
                          ]
                          (Type.namedWith
                               [ "Json", "Decode" ]
                               "Decoder"
                               [ Type.var "a" ]
                          )
                     )
             }
        )
        [ resolveArg_ ]


call_ :
    { required :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , requiredAt :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , optional :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , optionalAt :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , hardcoded : Elm.Expression -> Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression -> Elm.Expression
    , resolve : Elm.Expression -> Elm.Expression
    }
call_ =
    { required =
        \requiredArg_ requiredArg_0 requiredArg_1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "required"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.var "a" ]
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.function
                                            [ Type.var "a" ]
                                            (Type.var "b")
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "b" ]
                                  )
                             )
                     }
                )
                [ requiredArg_, requiredArg_0, requiredArg_1 ]
    , requiredAt =
        \requiredAtArg_ requiredAtArg_0 requiredAtArg_1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "requiredAt"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list Type.string
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.var "a" ]
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.function
                                            [ Type.var "a" ]
                                            (Type.var "b")
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "b" ]
                                  )
                             )
                     }
                )
                [ requiredAtArg_, requiredAtArg_0, requiredAtArg_1 ]
    , optional =
        \optionalArg_ optionalArg_0 optionalArg_1 optionalArg_2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "optional"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.var "a" ]
                                  , Type.var "a"
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.function
                                            [ Type.var "a" ]
                                            (Type.var "b")
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "b" ]
                                  )
                             )
                     }
                )
                [ optionalArg_, optionalArg_0, optionalArg_1, optionalArg_2 ]
    , optionalAt =
        \optionalAtArg_ optionalAtArg_0 optionalAtArg_1 optionalAtArg_2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "optionalAt"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list Type.string
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.var "a" ]
                                  , Type.var "a"
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.function
                                            [ Type.var "a" ]
                                            (Type.var "b")
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "b" ]
                                  )
                             )
                     }
                )
                [ optionalAtArg_
                , optionalAtArg_0
                , optionalAtArg_1
                , optionalAtArg_2
                ]
    , hardcoded =
        \hardcodedArg_ hardcodedArg_0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "hardcoded"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.var "a"
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.function
                                            [ Type.var "a" ]
                                            (Type.var "b")
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "b" ]
                                  )
                             )
                     }
                )
                [ hardcodedArg_, hardcodedArg_0 ]
    , custom =
        \customArg_ customArg_0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "custom"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.var "a" ]
                                  , Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.function
                                            [ Type.var "a" ]
                                            (Type.var "b")
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "b" ]
                                  )
                             )
                     }
                )
                [ customArg_, customArg_0 ]
    , resolve =
        \resolveArg_ ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Json", "Decode", "Pipeline" ]
                     , name = "resolve"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith
                                      [ "Json", "Decode" ]
                                      "Decoder"
                                      [ Type.namedWith
                                            [ "Json", "Decode" ]
                                            "Decoder"
                                            [ Type.var "a" ]
                                      ]
                                  ]
                                  (Type.namedWith
                                       [ "Json", "Decode" ]
                                       "Decoder"
                                       [ Type.var "a" ]
                                  )
                             )
                     }
                )
                [ resolveArg_ ]
    }


values_ :
    { required : Elm.Expression
    , requiredAt : Elm.Expression
    , optional : Elm.Expression
    , optionalAt : Elm.Expression
    , hardcoded : Elm.Expression
    , custom : Elm.Expression
    , resolve : Elm.Expression
    }
values_ =
    { required =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "required"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.var "a" ]
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.function [ Type.var "a" ] (Type.var "b") ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "b" ]
                         )
                    )
            }
    , requiredAt =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "requiredAt"
            , annotation =
                Just
                    (Type.function
                         [ Type.list Type.string
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.var "a" ]
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.function [ Type.var "a" ] (Type.var "b") ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "b" ]
                         )
                    )
            }
    , optional =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "optional"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.var "a" ]
                         , Type.var "a"
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.function [ Type.var "a" ] (Type.var "b") ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "b" ]
                         )
                    )
            }
    , optionalAt =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "optionalAt"
            , annotation =
                Just
                    (Type.function
                         [ Type.list Type.string
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.var "a" ]
                         , Type.var "a"
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.function [ Type.var "a" ] (Type.var "b") ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "b" ]
                         )
                    )
            }
    , hardcoded =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "hardcoded"
            , annotation =
                Just
                    (Type.function
                         [ Type.var "a"
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.function [ Type.var "a" ] (Type.var "b") ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "b" ]
                         )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.var "a" ]
                         , Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.function [ Type.var "a" ] (Type.var "b") ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "b" ]
                         )
                    )
            }
    , resolve =
        Elm.value
            { importFrom = [ "Json", "Decode", "Pipeline" ]
            , name = "resolve"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith
                             [ "Json", "Decode" ]
                             "Decoder"
                             [ Type.namedWith
                                   [ "Json", "Decode" ]
                                   "Decoder"
                                   [ Type.var "a" ]
                             ]
                         ]
                         (Type.namedWith
                              [ "Json", "Decode" ]
                              "Decoder"
                              [ Type.var "a" ]
                         )
                    )
            }
    }