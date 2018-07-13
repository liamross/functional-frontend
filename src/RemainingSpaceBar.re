type connector = {
  resourceType : string,
  used : int,
  teamId : string,
  regionId : int,
  repositoryId : string,
}

type state = {
  connectors: list(connector),
  error: string,
};

type action =
  | GetConnectors
  | GetSuccess(list(connector))
  | GetFailed;

let component = ReasonReact.reducerComponent("RemainingSpaceBar");

let make = (_children) => {
  ...component,

  initialState: () => {connectors: [], error: ""},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | LoadUsers =>
      ReasonReact.UpdateWithSideEffects(
        state,
        (
          self =>
            Js.Promise.(
              fetchUsers()
              |> then_(result =>
                   switch (result) {
                   | Some(users) =>
                      resolve(self.send(LoadedUsers(users)))
                   | None => resolve(self.send(LoadUsersFailed))
                   }
                 )
              |> ignore
            )
        ),
      )
    | GetSuccess(connectors) => ReasonReact.Update(Success(connectors))
    | GetFailed => ReasonReact.Update(Failure)

  didMount: (self) =>
    Js.Promise.(
      Axios.get("http://localhost:8001/api/v1/heroes")
      |> then_((response) => resolve(Js.log(response##data)))
      |> catch((error) => resolve(Js.log(error)))
    ),

  render: self => {
    let message =
      "You've clicked this " ++ string_of_int(self.state.count) ++ " times(s)";
    <div>
      <button onClick=(_event => self.send(Click))>
        (ReasonReact.string(message))
      </button>
      <button onClick=(_event => self.send(Toggle))>
        (ReasonReact.string("Toggle greeting"))
      </button>
      (self.state.show ? ReasonReact.string("hi") : ReasonReact.null)
    </div>;
  },
};
