{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-kernel"
, dependencies =
  [ "bifunctors"
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
  , "lists"
  , "maybe"
  , "newtype"
  , "parsing"
  , "partial"
  , "prelude"
  , "record"
  , "simple-json"
  , "transformers"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
