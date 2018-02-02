let placeFood = () => Js.Math.floor_int(Js.Math.random() *. (580. -. 0. +. 1.)) + 0;

type foodPoint = (int, int);

type playing =
  | NewGame
  | Play
  | Paused
  | GameOver;

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

let component = ReasonReact.reducerComponent("Canvas");

let make = (_children) => {
  ...component,
  initialState: () => {playing: NewGame, food: (placeFood(), placeFood()), score: 0},
  reducer: (action, state) =>
    switch action {
    | Start => ReasonReact.Update({...state, playing: Play})
    | Stop => ReasonReact.Update({...state, playing: Paused})
    | End => ReasonReact.Update({...state, playing: GameOver})
    | Reset => ReasonReact.Update({playing: Play, food: (placeFood(), placeFood()), score: 0})
    | UpdateScore =>
      ReasonReact.Update({...state, score: state.score + 1, food: (placeFood(), placeFood())})
    },
  render: ({send, state}) =>
    <div
      style=(
        ReactDOMRe.Style.make(
          ~display="flex",
          ~overflow="hidden",
          ~flexDirection="column",
          ~alignItems="center",
          ~justifyContent="center",
          ()
        )
      )>
      <div>
        <p style=(ReactDOMRe.Style.make(~fontSize="35px", ()))>
          (ReasonReact.stringToElement("Score: " ++ string_of_int(state.score)))
        </p>
      </div>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~overflow="hidden",
            ~position="relative",
            ~height="600px",
            ~width="600px",
            ~backgroundColor="rgb(27, 27, 27)",
            ()
          )
        )>
        (
          switch state.playing {
          | NewGame =>
            <h2
              style=(
                ReactDOMRe.Style.make(
                  ~display="flex",
                  ~alignSelf="center",
                  ~justifyContent="center",
                  ~zIndex="10",
                  ~fontSize="55px",
                  ~fontWeight="bold",
                  ~color="#fff",
                  ()
                )
              )>
              (ReasonReact.stringToElement("Press SPACE to start"))
            </h2>
          | Play => ReasonReact.nullElement
          | Paused =>
            <h2
              style=(
                ReactDOMRe.Style.make(
                  ~display="flex",
                  ~alignSelf="center",
                  ~justifyContent="center",
                  ~zIndex="10",
                  ~fontSize="55px",
                  ~fontWeight="bold",
                  ~color="#fff",
                  ()
                )
              )>
              (ReasonReact.stringToElement("Paused"))
            </h2>
          | GameOver =>
            <h2
              style=(
                ReactDOMRe.Style.make(
                  ~display="flex",
                  ~alignSelf="center",
                  ~justifyContent="center",
                  ~zIndex="10",
                  ~fontSize="55px",
                  ~fontWeight="bold",
                  ~color="#fff",
                  ()
                )
              )>
              (ReasonReact.stringToElement("Game Over"))
            </h2>
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
                | Play => Stop
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