type playing =
  | NewGame
  | Playing
  | Paused
  | GameOver;

type foodPoint = (int, int);

type state = {
  playing,
  food: foodPoint,
  score: int
};

type action =
  | Start
  | Stop
  | End
  | Reset
  | UpdateScore;

let gameContainerCss =
  ReactDOMRe.Style.make(
    ~display="flex",
    ~overflow="hidden",
    ~flexDirection="column",
    ~alignItems="center",
    ~justifyContent="center",
    ()
  );

let canvasCss =
  ReactDOMRe.Style.make(
    ~overflow="hidden",
    ~position="relative",
    ~height="600px",
    ~width="600px",
    ~backgroundColor="rgb(27, 27, 27)",
    ()
  );

let gameTextCss =
  ReactDOMRe.Style.make(
    ~display="flex",
    ~alignSelf="center",
    ~justifyContent="center",
    ~zIndex="10",
    ~fontSize="55px",
    ~fontWeight="bold",
    ~color="#fff",
    ()
  );

let placeFood = () => Js.Math.floor_int(Js.Math.random() *. (580. -. 0. +. 1.)) + 0;

let component = ReasonReact.reducerComponent("Canvas");

let make = (_children) => {
  ...component,
  initialState: () => {playing: NewGame, food: (placeFood(), placeFood()), score: 0},
  reducer: (action, state) =>
    switch action {
    | Start => ReasonReact.Update({...state, playing: Playing})
    | Stop => ReasonReact.Update({...state, playing: Paused})
    | End => ReasonReact.Update({...state, playing: GameOver})
    | Reset => ReasonReact.Update({playing: Playing, food: (placeFood(), placeFood()), score: 0})
    | UpdateScore =>
      ReasonReact.Update({...state, score: state.score + 1, food: (placeFood(), placeFood())})
    },
  render: ({send, state}) =>
    <div style=gameContainerCss>
      <div>
        <p style=(ReactDOMRe.Style.make(~fontSize="35px", ()))>
          (ReasonReact.stringToElement("Score: " ++ string_of_int(state.score)))
        </p>
      </div>
      <div style=canvasCss>
        (
          switch state.playing {
          | NewGame =>
            <h2 style=gameTextCss> (ReasonReact.stringToElement("Press SPACE to start")) </h2>
          | Playing => ReasonReact.nullElement
          | Paused => <h2 style=gameTextCss> (ReasonReact.stringToElement("Paused")) </h2>
          | GameOver => <h2 style=gameTextCss> (ReasonReact.stringToElement("Game Over")) </h2>
          }
        )
        <Snake
          food=state.food
          eat=(() => send(UpdateScore))
          pause=(
            () =>
              send(
                switch state.playing {
                | NewGame => Start
                | Playing => Stop
                | Paused => Start
                | GameOver => Reset
                }
              )
          )
          gameOver=(() => send(End))
        />
        <Food position=state.food />
      </div>
    </div>
};