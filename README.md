# elm-codegen-feature-flags

An Elm code generation library built on top of
[elm-codegen](https://package.elm-lang.org/packages/mdgriffith/elm-codegen/latest/) to help you
generate feature flags without any boilerplate.

By "feature flags", we're talking about a record with a bunch of primitive types, mostly booleans.
Something like:

```elm
type alias FeatureFlags =
  { animateSignInButton : Bool
  , useNewFooter : Bool
  }
```

With minimal config, this package aims to make it easy (and safe!) to maintain, extend, and reduce
these records without thinking hard (or, ideally, at all).

## Table of contents

- [Installation](#installation)
- [Why use feature flags?](#why-use-feature-flags-)
- [What's hard about them?](#what-s-hard-about-them-)
- [What this library generates](#what-this-library-generates)
- [Feedback](#feedback)

## Installation

Like any Elm package, you're going to need Elm, and a project to install stuff in! After that:

1. Install [elm-codegen](https://github.com/mdgriffith/elm-codegen)
2. Set up a Generate.elm file (elm-codegen CLI will do this for you automatically)
3. Within your `codegen/` directory, run `elm install fakemonster/elm-codegen-feature-flags`


## Why use feature flags?

Let's say you're working on a big new feature for your website. You've already written a ton of
code, and you'd like to deploy it and let people take a look, but it's not ready for the general
public yet.

A common convention is to use "feature flags": runtime configuration (usually passed as url
parameters or json objects!) that you key off of to activate your alternative experience. They're
usually pretty simple, and I often see them implemented as a big record of primitive values. Pretty
much always booleans, sometimes a string for using a new API version or something.

## What's hard about them?

Over time, especially on a big project, these records can get unwieldy:

```elm
type alias FeatureFlags =
    { customApiDomain : Maybe String
    , eagerLoadInvites : Bool
    , largeLoginButton : Bool
    , largeSignupButton : Bool
    , useExperimentalAnimationLibrary : Bool
    -- we won't add a sixth one. right?
    }
```

And they can become error-prone when you've got to serialize and deserialize them, or you copy-paste
a bit too much adding a new one:

```elm
decoder =
    map5
        (field "customApiDomain" (JD.maybe JD.string))
        (field "eagerLoadInvites" JD.bool)
        (field "largeSignupButton" JD.bool)
        (field "largeLoginButton" JD.bool) -- wrong order!!
        (field "experimentalAnimationLibary" JD.bool) -- typo!
```

Now, in our applications we have ways of preventing many of these issues. We could use custom types
instead of booleans, or tag all of them with phantom types to avoid mixups. We could use functions
or clever type aliases to get the compiler to guide us through the decoder, encoder etc. We could
write tests! And I strongly encourage you to do all that in the _interesting_ parts of your
application.

But feature flags aren't interesting! They're a common pattern, it's a pile of data for powering
`if` expressions, and the only thing custom to you is the field name, at the end of the day. **So
why not codegen them?**

## What this library generates

Straightforwardly, this is a codegen module that can make you an arbitrarily large record full of
`Bools` and `Maybe String`s. The generated code has some utility functions that I've found useful in
working with feature flags. At your discretion, it also mixes in encoding/decoding for JSON and/or
URL querystrings. If you're being meticulous, you can note that other than the alias name I've
picked, you could use this for any kind of record where all the types are `Bool` or `Maybe String`.

Used in elm-codegen, generally it'll look something like this:

```elm
module Generate exposing (main)

import Gen.CodeGen.Generate as Generate
import CodeGen.FeatureFlags as FF


main : Program {} () ()
main =
    Generate.run
        [ FF.fromFlags
            [ ( "customApiDomain", FF.maybeString )
            , ( "welcomeMessage", FF.string "Welcome!" )
            , ( "eagerLoadInvites", FF.bool )
            , ( "largeLoginButton", FF.bool )
            , ( "largeSignupButton", FF.bool )
            , ( "useExperimentalAnimationLibrary", FF.bool )
            ]
            |> FF.withJsonConverters
            |> FF.withUrlConverters
            |> FF.withQueryKeyFormatter (\s -> "f-" ++ s)
            |> FF.generate "MyFeatureFlags"
        ]
```

This generates a module named `MyFeatureFlags` with:
- a `FeatureFlags` alias containing your given fields and types
- JSON and URL querystring encoders and decoders
- a setter for each field
- utility functions and types to do common operations safely

## Following along

The `examples` directory shows an application using FeatureFlags in practice. Clone this repo, and
then in the `examples` directory:

1. `npx elm-codegen install`
2. `npx elm-codegen run --output generated`

You can also use `elm-doc-preview` to see documentation for the _generated_ code as well! All these
commands are in the Makefile as well.

## Feedback

The code generated here is based on my experience with feature flags, and the types of
transformation and maintenance that I've had to do with them. If you have other utilities you'd be
interested in, or support for additional types, please open an issue to let me know.
