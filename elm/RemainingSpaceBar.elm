module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Record
import MockApi
import MockHttp


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Connector =
    { resourceType : String
    , used : Int
    }


type Status
    = Success
    | Loading
    | Error String


type alias Model =
    { connectors : List Connector
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] Loading, getConnectors )



-- UPDATE


type Msg
    = ConnectorsResponse (Result Http.Error (List Connector))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConnectorsResponse (Ok connectors) ->
            ( { model | connectors = connectors, status = Success }, Cmd.none )

        ConnectorsResponse (Err _) ->
            ( { model | connectors = [], status = Error "Error." }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        modelBody =
            case model.status of
                Loading ->
                    Html.text "Loading..."

                Error text ->
                    Html.text text

                Success ->
                    div
                        [ class "storage-used__display" ]
                        (List.indexedMap
                            (\idx connector -> connectorBar idx connector model)
                            model.connectors
                        )
    in
    div
        [ class "storage-used" ]
        [ div
            [ class "storage-used__main" ]
            [ modelBody
            ]
        ]


connectorBar : Int -> Connector -> Model -> Html msg
connectorBar index connector model =
    let
        connectorShadow =
            if index == -1 then
                ( "box-shadow", "none" )
            else
                ( "box-shadow", "inset 0 -1px 0 rgba(0, 0, 0, .15)" )

        connectorBackgroundColor =
            if index == -1 then
                "transparent"
            else
                getConnectorColor connector.resourceType

        connectorWidth =
            model.connectors
                |> getTotalStored
                |> getStorageLimit
                |> getConnectorWidth connector
    in
    div
        [ class "storage-used__connector"
        , style
            (connectorShadow
                :: [ ( "backgroundColor", connectorBackgroundColor )
                   , ( "width", connectorWidth )
                   ]
            )
        ]
        []



-- VIEW UTILITIES


getTotalStored : List Connector -> Int
getTotalStored connectors =
    List.foldr (\connector acc -> connector.used + acc) 0 connectors


getStorageLimit : Int -> Int
getStorageLimit totalStored =
    totalStored
        |> toFloat
        |> divide (1024 ^ 4)
        |> ceiling
        |> multiply (1024 ^ 4)
        |> max (1024 ^ 4)


getConnectorWidth : Connector -> Int -> String
getConnectorWidth connector storageLimit =
    (toFloat connector.used / toFloat storageLimit)
        |> multiply 100
        |> toString
        |> stringAppend "%"


getConnectorColor : String -> String
getConnectorColor connector =
    if connector == "SP" then
        "#2488d8"
    else if connector == "Exchange" then
        "#106ebe"
    else if connector == "Exports" then
        "#00b7c3"
    else
        "lightblue"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getConnectors : Cmd Msg
getConnectors =
    let
        url =
            "https://test.ca/connectors"

        createRequest url =
            MockHttp.get url connectorsDecoder

        sendRequest request =
            MockHttp.send MockApi.fetchConnectors ConnectorsResponse request
    in
    url |> createRequest |> sendRequest


connectorsDecoder : Decode.Decoder (List Connector)
connectorsDecoder =
    Decode.list connectorDecoder


connectorDecoder : Decode.Decoder Connector
connectorDecoder =
    Record.decode Connector
        |> Record.required "resourceType" Decode.string
        |> Record.required "used" Decode.int



-- HELPERS


multiply : number -> number -> number
multiply a b =
    a * b


divideInt : Int -> Int -> Float
divideInt a b =
    toFloat a / toFloat b


divide : Float -> Float -> Float
divide a b =
    a / b


stringAppend : String -> String -> String
stringAppend a b =
    a ++ b
