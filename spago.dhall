{ name =
    "purescript-erl-maps"
, backend =
    "purerl"
, dependencies =
    [ "prelude", "erl-atom", "erl-lists" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
