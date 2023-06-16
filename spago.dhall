{ name = "purescript"
, dependencies =
  [ "generate-values"
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
