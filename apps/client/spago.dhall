{ name = "snipts-client"
, dependencies = [ "console", "effect", "halogen", "prelude", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
