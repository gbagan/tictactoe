let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230616/packages.dhall
        sha256:d15d99463bdce08f78a80bd8cde1fadb2ce1b490eb53924adf44215713f1a1a5

let overrides = {=}

let additions =
      { pha =
        { dependencies =
          [ "aff"
          , "effect"
          , "free"
          , "prelude"
          , "unsafe-reference"
          , "web-uievents"
          , "web-pointerevents"
          ]
        , repo = "https://github.com/gbagan/purescript-pha.git"
        , version = "master"
        }
      , relude =
        { dependencies =
          [ "aff"
          , "arrays"
          , "control"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "generate-values"
          , "integers"
          , "lazy"
          , "lists"
          , "maybe"
          , "numbers"
          , "ordered-collections"
          , "prelude"
          , "profunctor-lenses"
          , "transformers"
          , "tuples"
          , "unfoldable"
          ]
        , repo = "https://github.com/gbagan/purescript-relude.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
