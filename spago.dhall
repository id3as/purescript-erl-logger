{ name =
    "purescript-erl-maps"
, backend =
    "purerl"
, dependencies =
    [ "prelude", "erl-atom", "erl-lists", "record" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
