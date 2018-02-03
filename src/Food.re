let component = ReasonReact.statelessComponent("Food");

/*
   Food takes a position (x, y) and then renders on that coordinates
 */
let make = (~position, _children) => {
  ...component,
  render: (_self) => {
    let (x, y) = position;
    <div
      style=(
        ReactDOMRe.Style.make(
          ~position="absolute",
          ~top=string_of_int(y) ++ "px",
          ~left=string_of_int(x) ++ "px",
          ~height="30px",
          ~width="30px",
          ~backgroundColor="#ff6b6b",
          ()
        )
      )
    />
  }
};