/*
   Snake Condition
 */
type condition =
  | Alive
  | Paused
  | Dead;

/*
   Input keys to register
 */
type key =
  | Space
  | Left
  | Right
  | Up
  | Down
  | Noop;

/*
 Food point (x, y)
 */
type foodPoint = (int, int);

/* (x, y, Key, [(x, y, Key)])<TurnPoints> */
type point = (int, int, key, array((int, int, key)));

type state = {
  points: array(point),
  condition
};

/*
   Type of Game Actions
 */
type action =
  | Move
  | KeyPress(key)
  | Eat
  | Kill;

/*
   Turn int representation of key into Tag
 */
let codeToKey = (event) => {
  let code = ReactEventRe.Keyboard.which(event);
  switch code {
  | 32 => Space
  | 37 => Left
  | 39 => Right
  | 38 => Up
  | 40 => Down
  | _ => Noop
  }
};

/*
   - Grab last Item in List
   - use it's x and y and turnpoints in order to determine where to place it
 */
let grow = (points: array(point)) => {
  let (x, y, direction, turnPoints) = points[Array.length(points) - 1];
  switch direction {
  | Left => [|(x + 30, y, direction, turnPoints)|]
  | Right => [|(x - 30, y, direction, turnPoints)|]
  | Up => [|(x, y + 30, direction, turnPoints)|]
  | Down => [|(x, y - 30, direction, turnPoints)|]
  | Space
  | Noop => [|(x, y, direction, turnPoints)|]
  }
};

/*
   If the character is dead don't turn anything other wise turn
 */
let turn = (state, next: key) =>
  switch state.condition {
  | Dead => state
  | Paused
  | Alive =>
    let (xh, yh, _, _) = state.points[0];
    let nextPoints =
      Array.map(
        ((x, y, direction, turnPoints)) =>
          switch next {
          | Left => (x, y, direction, Array.append(turnPoints, [|(xh, yh, Left)|]))
          | Right => (x, y, direction, Array.append(turnPoints, [|(xh, yh, Right)|]))
          | Up => (x, y, direction, Array.append(turnPoints, [|(xh, yh, Up)|]))
          | Down => (x, y, direction, Array.append(turnPoints, [|(xh, yh, Down)|]))
          | Space
          | Noop => (x, y, direction, turnPoints)
          },
        state.points
      );
    {...state, points: nextPoints}
  };

let component = ReasonReact.reducerComponent("Snake");

let make = (~gameOver, ~pause, ~eat, ~food: foodPoint, _children) => {
  ...component,
  initialState: () => {condition: Paused, points: [|(270, 270, Right, [||])|]},
  reducer: (action, state) =>
    switch action {
    | Move =>
      switch state.condition {
      | Paused
      | Dead => ReasonReact.NoUpdate
      | Alive =>
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            points:
              Array.map(
                ((x, y, direction, turnPoints)) =>
                  switch direction {
                  | Left =>
                    Array.length(turnPoints) == 0 ?
                      (x - 1, y, Left, turnPoints) :
                      {
                        let (xTop, _yTop, keyTop) = turnPoints[0];
                        switch keyTop {
                        | Up =>
                          x <= xTop ?
                            (
                              x,
                              y - 1,
                              Up,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x - 1, y, Left, turnPoints)
                        | Down =>
                          x <= xTop ?
                            (
                              x,
                              y + 1,
                              Down,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x - 1, y, Left, turnPoints)
                        | _ => (
                            x - 1,
                            y,
                            Left,
                            Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                          )
                        }
                      }
                  | Right =>
                    Array.length(turnPoints) == 0 ?
                      (x + 1, y, Right, turnPoints) :
                      {
                        let (xTop, _yTop, keyTop) = turnPoints[0];
                        switch keyTop {
                        | Up =>
                          x >= xTop ?
                            (
                              x,
                              y - 1,
                              Up,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x + 1, y, Right, turnPoints)
                        | Down =>
                          x >= xTop ?
                            (
                              x,
                              y + 1,
                              Down,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x + 1, y, Right, turnPoints)
                        | _ => (
                            x + 1,
                            y,
                            Right,
                            Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                          )
                        }
                      }
                  | Up =>
                    Array.length(turnPoints) == 0 ?
                      (x, y - 1, Up, turnPoints) :
                      {
                        let (_xTop, yTop, keyTop) = turnPoints[0];
                        switch keyTop {
                        | Left =>
                          y <= yTop ?
                            (
                              x - 1,
                              y,
                              Left,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x, y - 1, Up, turnPoints)
                        | Right =>
                          y <= yTop ?
                            (
                              x + 1,
                              y,
                              Right,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x, y - 1, Up, turnPoints)
                        | _ => (
                            x,
                            y - 1,
                            Up,
                            Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                          )
                        }
                      }
                  | Down =>
                    Array.length(turnPoints) == 0 ?
                      (x, y + 1, Down, turnPoints) :
                      {
                        let (_xTop, yTop, keyTop) = turnPoints[0];
                        switch keyTop {
                        | Left =>
                          y >= yTop ?
                            (
                              x - 1,
                              y,
                              Left,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x, y + 1, Down, turnPoints)
                        | Right =>
                          y >= yTop ?
                            (
                              x + 1,
                              y,
                              Right,
                              Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                            ) :
                            (x, y + 1, Down, turnPoints)
                        | _ => (
                            x,
                            y - 1,
                            Down,
                            Array.sub(turnPoints, 1, Array.length(turnPoints) - 1)
                          )
                        }
                      }
                  | Space
                  | Noop => (x, y, direction, turnPoints)
                  },
                state.points
              )
          },
          (
            ({state, send}) => {
              let count = ref(0);
              let (xh, yh, _, _) = state.points[0];
              let (x2, y2) = food;
              Array.iter(
                ((x, y, _, _)) =>
                  x <= 584 && x >= 0 && y <= 584 && y >= 0 ?
                    Array.length(state.points) > 1 && abs(xh - x) <= 10 && abs(yh - y) <= 10 ?
                      switch count^ {
                      | 0 => count := count^ + 1
                      | _ => send(Kill)
                      } :
                      abs(x2 - x) <= 25 && abs(y2 - y) <= 25 ?
                        {
                          send(Eat);
                          eat()
                        } :
                        () :
                    {
                      send(Kill);
                      ()
                    },
                state.points
              );
              ()
            }
          )
        )
      }
    | KeyPress(Space) =>
      ReasonReact.UpdateWithSideEffects(
        switch state.condition {
        | Alive => {...state, condition: Paused}
        | Paused => {...state, condition: Alive}
        | Dead => {condition: Alive, points: [|(270, 270, Right, [||])|]}
        },
        ((_self) => pause())
      )
    | KeyPress(Left) => ReasonReact.Update(turn(state, Left))
    | KeyPress(Right) => ReasonReact.Update(turn(state, Right))
    | KeyPress(Up) => ReasonReact.Update(turn(state, Up))
    | KeyPress(Down) => ReasonReact.Update(turn(state, Down))
    | KeyPress(Noop) => ReasonReact.NoUpdate
    | Eat => ReasonReact.Update({...state, points: Array.append(state.points, grow(state.points))})
    | Kill =>
      ReasonReact.UpdateWithSideEffects({...state, condition: Dead}, ((_state) => gameOver()))
    },
  subscriptions: (self) => [
    Sub(() => Js.Global.setInterval(() => self.send(Move), 5), Js.Global.clearInterval)
  ],
  render: ({send, state}) => {
    let count = ref((-1));
    <div>
      (
        ReasonReact.arrayToElement(
          Array.map(
            ((x, y, _direction, _turnPoints)) => {
              count := count^ + 1;
              <div
                key=(string_of_int(count^))
                style=(
                  ReactDOMRe.Style.make(
                    ~position="absolute",
                    ~top=string_of_int(y) ++ "px",
                    ~left=string_of_int(x) ++ "px",
                    ~height="30px",
                    ~width="30px",
                    ~fontSize="7px",
                    ~backgroundColor="#70ff71",
                    ()
                  )
                )
              />
            },
            state.points
          )
        )
      )
      <input
        onKeyDown=((e) => send(KeyPress(codeToKey(e))))
        autoFocus=Js.true_
        style=(
          ReactDOMRe.Style.make(
            ~width="0px",
            ~height="0px",
            ~position="relative",
            ~top="100px",
            ~zIndex="-100",
            ()
          )
        )
      />
    </div>
  }
};