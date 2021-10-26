let upstream =
            https://github.com/purerl/package-sets/releases/download/erl-0.14.4-20211012-1/packages.dhall sha256:04b7cb6aaf4cc7323c2560c7b5c2f5e8459d2951997cf5084748e0f1cdbabd26


let overrides =
      { erl-test-eunit =
          { dependencies =
              [ "assert"
              , "console"
              , "debug"
              , "erl-lists"
              , "erl-tuples"
              , "foreign"
              , "free"
              , "prelude"
              , "psci-support"
              ]
          , repo =
              "ssh://git@github.com/id3as/purescript-erl-test-eunit.git"
          , version =
              "ae8e2573851a70eb44199ae1eef7dcb8537184d5"
          }
      }

let additions = {=}

in  upstream ⫽ overrides ⫽ additions
