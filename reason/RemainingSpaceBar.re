open Belt;

type connector = {
  resourceType: string,
  used: int,
};

type state =
  | Loading
  | Error(string)
  | Loaded(array(connector));

type action =
  | ConnectorsFetch
  | ConnectorsFetched(array(connector))
  | ConnectorsFailedToFetch;

let component = ReasonReact.reducerComponent("RemainingSpaceBar");

let make = _children => {
  ...component,
  initialState: _state => Loading,
  reducer: (action, _state) =>
    switch (action) {
    | ConnectorsFetch =>
      ReasonReact.UpdateWithSideEffects(
        Loading,
        (
          self =>
            Js.Promise.(
              Fetch.fetch("https://test.ca/connectors")
              |> then_(Fetch.Response.json)
              |> then_(json =>
                   json
                   |> Decode.connectors
                   |> (
                     connectors => self.send(ConnectorsFetched(connectors))
                   )
                   |> resolve
                 )
              |> catch(_err =>
                   Js.Promise.resolve(self.send(ConnectorsFailedToFetch))
                 )
              |> ignore
            )
        ),
      )
    | ConnectorsFetched(connectors) =>
      ReasonReact.Update(Loaded(connectors))
    | ConnectorsFailedToFetch => ReasonReact.Update(Error("Error."))
    },
  didMount: self => self.send(ConnectorsFetch),
  render: self =>
    <div className="storage-used">
      <div className="storage-used__main">
        (
          switch (self.state) {
          | Error(error) => ReasonReact.string(error)
          | Loading => ReasonReact.string("Loading...")
          | Loaded(connectors) =>
            <div className="storage-used__display">
              (
                connectors
                |> ReasonReact.array(
                     Array.of_list(
                       List.mapi(
                         (index, connector) => {
                           let connectorShadow =
                             if (index === (-1)) {
                               "none";
                             } else {
                               "inset 0 -1px 0 rgba(0, 0, 0, .15)";
                             };

                           let connectorBackgroundColor =
                             if (index === (-1)) {
                               "transparent";
                             } else {
                               getConnectorColor(connector.resourceType);
                             };

                           let connectorWidth =
                              model.connectors
                                  |> getTotalStored
                                  |> getStorageLimit
                                  |> getConnectorWidth(connector);

                           <div
                             className="storage-used__connector"
                             style=(
                               ReactDOMRe.Style.make(
                                 ~boxShadow=connectorShadow,
                                 ~backgroundColor=connectorBackgroundColor,
                                 ~width="68px",
                                 (),
                               )
                             )
                           />;
                         },
                         connectors,
                       ),
                     ),
                   )
              )
            </div>
          }
        )
      </div>
    </div>,
};

module Decode = {
  let connectors = json : list(connector) =>
    json
    |> List.map(_, connector =>
         Json.Decode.{
           resourceType: connector |> field("resourceType", string),
           used: connector |> field("used", int),
         }
       );
};


let getTotalStored = (connectors) : int =>
  List.fold_right(((connector, acc) => connector.used + acc), connectors, 0);


let getStorageLimit = (totalStored) : int =>
  totalStored
  |> Int32.to_float
  |> divide(Complex.pow(1024. 4))
  |> Complex.ceil
  |> multiply(Complex.pow(1024. 4))
  |> max(Complex.pow(1024. 4));


let getConnectorWidth = (connector, storageLimit) : string =>
  (toFloat connector.used / toFloat storageLimit)
  |> multiply(100)
  |> toString
  |> stringAppend("%");


let getConnectorColor = connector : string =>
  if (connector == "SP") {
    "#2488d8";
  } else if (connector == "Exchange") {
    "#106ebe";
  } else if (connector == "Exports") {
    "#00b7c3";
  } else {
    "lightblue";
  };

let multiply = (a, b) : int => a * b;

let divideInt = (a, b) : float => float_of_int(a) / float_of_int(b);

let divide = (a, b) : float => a / b;

let stringAppend = (a, b) : string => a ++ b;