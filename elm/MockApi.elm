module MockApi exposing (fetchConnectors)

import MockHttp exposing (Endpoint(..))


fetchConnectors : MockHttp.Config
fetchConnectors =
    let
        endpoints =
            [ Get
                { url = "https://test.ca/connectors"
                , responseTime = 800
                , response = """
                    [
                        {
                            "resourceType":"Exchange",
                            "used":448076898
                        },
                        {
                            "resourceType":"SP",
                            "used":922683
                        },
                        {
                            "resourceType":"Exports",
                            "used":8448922683
                        }
                    ]
                """
                }
            ]
    in
    MockHttp.config endpoints
