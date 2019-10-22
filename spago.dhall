{ name =
    "nodetrout-realworld"
, dependencies =
    [ "console", "effect", "nodetrout", "psci-support", "trout" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
