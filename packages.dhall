let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.3-20191005/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20191005/packages.dhall sha256:ba287d858ada09c4164792ad4e643013b742c208cbedf5de2e35ee27b64b6817

let overrides = {=}

let additions =
      { nodetrout =
          mkPackage
            [ "argonaut"
            , "effect"
            , "form-urlencoded"
            , "http-methods"
            , "node-http"
            , "prelude"
            , "profunctor-lenses"
            , "trout"
            ]
            "https://github.com/nsaunders/purescript-nodetrout.git"
            "ca465ef18832ce3c8b6faedc2ddca2faabb1adae"
      , trout =
          mkPackage
            [ "argonaut", "media-types", "prelude", "smolder", "uri" ]
            "https://github.com/nsaunders/purescript-trout.git"
            "4edde87293e3905817bfa184e1434be5d2989f39"
      }

in  upstream // overrides // additions
