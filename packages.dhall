let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.5-20220204-2/packages.dhall sha256:bf284d597ad053b43591b964a52aa0f41ed12a576c3efde85ba999ad65072fc9

in upstream
  with simple-json.version = "19b092b4bea82b5751169db81de53bd4addb3402"
  with simple-json.repo = "https://github.com/id3as/purescript-simple-json.git"
  with erl-simplebus.version = "1a2977d92c65b8c3c99ee2ac79420a16f609de7e"
  with erl-kernel.version = "f93533a79fa4cc04d90985b6aca18c8a84909d98"
  with erl-simplebus.version = "0b88c5686d6218a5619416e1550d6809b1b3670d"
  with erl-pinto.version = "55ded43bf38904706fae8978c230713de9c6062f"

  with datetime-parsing =
    { repo = "https://github.com/flounders/purescript-datetime-parsing"
    , dependencies =
            [  "arrays"
             , "datetime"
             , "either"
             , "enums"
             , "foldable-traversable"
             , "integers"
             , "lists"
             , "maybe"
             , "numbers"
             , "parsing"
             , "prelude"
             , "psci-support"
             , "strings"
            ]
    , version = "10c0a9aecc60a2a5e8cff35bebe45be4dacaa7f8"
    }
