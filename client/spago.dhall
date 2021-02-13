{ name = "snips-client"
, dependencies =
    [ "halogen"
    , "psci-support"
    , "aff"
    , "aff-bus"
    , "affjax"
    , "argonaut-core"
    , "codec-argonaut"
    , "console"
    , "debug"
    , "effect"
    , "formatters"
    , "halogen"
    , "halogen-formless"
    , "nonempty"
    , "now"
    , "precise-datetime"
    , "prelude"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "slug"
    , "variant" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
