{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-kernel"
, dependencies =
  [ "bifunctors"
  , "control"
  , "datetime"
  , "datetime-parsing"
  , "debug"
  , "effect"
  , "either"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "foldable-traversable"
  , "foreign"
  , "lists"
  , "maybe"
  , "newtype"
  , "parsing"
  , "partial"
  , "prelude"
  , "simple-json"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
