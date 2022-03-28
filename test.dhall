let config = ./spago.dhall
in config
    with dependencies = config.dependencies # [ "assert" ]
    with sources = config.sources # [ "test/**/*.purs" ]
    with name = "generate-values-tests"
