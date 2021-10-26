{ name = "purescript-erl-maps"
, backend = "purerl"
, dependencies = [ "effect", "erl-atom", "erl-lists", "prelude", "record" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
