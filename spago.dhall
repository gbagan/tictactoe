{ name = "purescript"
, dependencies =
  [ "arrays"
  , "generate-values"
  , "lcg"
  , "numbers"
  , "pha"
  , "refs"
  , "relude"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
