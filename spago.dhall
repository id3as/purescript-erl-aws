{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-kernel"
, dependencies =
  [ "bifunctors"
  , "control"
  , "convertable-options"
  , "datetime"
  , "datetime-parsing"
  , "debug"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "erl-process"
  , "erl-tuples"
  , "erl-untagged-union"
  , "foldable-traversable"
  , "foreign"
  , "functions"
  , "identity"
  , "integers"
  , "jsonld"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "record"
  , "simple-json"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
