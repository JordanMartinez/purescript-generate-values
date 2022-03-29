let config = ./spago.dhall
in config
    with dependencies = config.dependencies # [ "assert", "console" ]
    with sources = config.sources # [ "test/**/*.purs" ]
    with name = "generate-values-tests"
