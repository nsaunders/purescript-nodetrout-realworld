{ name =
    "nodetrout-realworld"
, dependencies =
    [ "console"
    , "crypto"
    , "effect"
    , "nodetrout"
    , "psci-support"
    , "querydsl"
    , "random"
    , "simple-jwt"
    , "trout"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
