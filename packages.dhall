let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

in  upstream
 with convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
 with erl-untagged-union =
        { repo = "https://github.com/id3as/purescript-erl-untagged-union.git"
        , dependencies =
        [ "erl-atom"
        , "erl-binary"
        , "erl-lists"
        , "erl-tuples"
        , "debug"
        , "foreign"
        , "typelevel-prelude"
        , "maybe"
        , "partial"
        , "prelude"
        , "unsafe-coerce"
        ]
        ,   version = "eb7a10c7930c4b99f1a6bfce767daa814d45dd2b"
        }
 with erl-kernel =
  { repo = "https://github.com/id3as/purescript-erl-kernel"
  , dependencies =
    [ "convertable-options"
    , "datetime"
    , "effect"
    , "either"
    , "erl-atom"
    , "erl-binary"
    , "erl-lists"
    , "erl-process"
    , "erl-tuples"
    , "erl-untagged-union"
    , "foldable-traversable"
    , "foreign"
    , "functions"
    , "integers"
    , "maybe"
    , "newtype"
    , "partial"
    , "prelude"
    , "record"
    , "typelevel-prelude"
    , "unsafe-coerce"
    ]
  , version = "2c1f78a3aa6993e91e342a984c522b87b98bbb2b"
  }
