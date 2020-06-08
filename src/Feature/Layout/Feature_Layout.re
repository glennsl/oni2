// MODEL

type model = {
  width: int,
  height: int,
  tree: Layout.t(int),
};

let initial = id => {width: 100, height: 100, tree: Layout.singleton(id)};

let updateTree = (f, model) => {...model, tree: f(model.tree)};

let windows = model => Layout.windows(model.tree);
let addWindow = (direction, focus) =>
  updateTree(Layout.addWindow(direction, focus));
let insertWindow = (target, direction, focus) =>
  updateTree(Layout.insertWindow(target, direction, focus));
let removeWindow = target => updateTree(Layout.removeWindow(target));

let move = (focus, dirX, dirY, layout) => {
  let positioned = Positioned.fromLayout(0, 0, 200, 200, layout);

  Positioned.move(focus, dirX, dirY, positioned)
  |> Option.value(~default=focus);
};

let moveLeft = current => move(current, -1, 0);
let moveRight = current => move(current, 1, 0);
let moveUp = current => move(current, 0, -1);
let moveDown = current => move(current, 0, 1);

// UPDATE

[@deriving show({with_path: false})]
type command =
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | RotateForward
  | RotateBackward
  | DecreaseSize
  | IncreaseSize
  | DecreaseHorizontalSize
  | IncreaseHorizontalSize
  | DecreaseVerticalSize
  | IncreaseVerticalSize
  | ResetSizes;

[@deriving show({with_path: false})]
type msg =
  | HandleDragged({
      path: list(int),
      delta: int,
    })
  | DimensionsChanged({
      width: int,
      height: int,
    })
  | Command(command);

type outmsg =
  | Nothing
  | Focus(int);

let rotate = (direction, focus, model) => {
  ...model,
  tree: Layout.rotate(direction, focus, model.tree),
};

let resizeWindow = (direction, focus, delta, {width, height, tree} as model) => {
  ...model,
  tree: Layout.resizeWindow(~width, ~height, direction, focus, delta, tree),
};

let resizeSplit = (~path, ~delta, {width, height, tree} as model) => {
  ...model,
  tree: Layout.resizeSplit(~width, ~height, ~path, ~delta, tree),
};

let resetWeights = model => {
  ...model,
  tree: Layout.resetWeights(model.tree),
};

let update = (~focus, model, msg) => {
  switch (msg) {
  | HandleDragged({path, delta}) => (
      resizeSplit(~path, ~delta, model),
      Nothing,
    )

  | DimensionsChanged({width, height}) => (
      {...model, width, height},
      Nothing,
    )

  | Command(MoveLeft) =>
    switch (focus) {
    | Some(focus) => (model, Focus(moveLeft(focus, model.tree)))
    | None => (model, Nothing)
    }

  | Command(MoveRight) =>
    switch (focus) {
    | Some(focus) => (model, Focus(moveRight(focus, model.tree)))
    | None => (model, Nothing)
    }

  | Command(MoveUp) =>
    switch (focus) {
    | Some(focus) => (model, Focus(moveUp(focus, model.tree)))
    | None => (model, Nothing)
    }

  | Command(MoveDown) =>
    switch (focus) {
    | Some(focus) => (model, Focus(moveDown(focus, model.tree)))
    | None => (model, Nothing)
    }

  | Command(RotateForward) =>
    switch (focus) {
    | Some(focus) => (rotate(`Forward, focus, model), Nothing)
    | None => (model, Nothing)
    }

  | Command(RotateBackward) =>
    switch (focus) {
    | Some(focus) => (rotate(`Backward, focus, model), Nothing)
    | None => (model, Nothing)
    }

  | Command(DecreaseSize) =>
    switch (focus) {
    | Some(focus) => (
        model
        |> resizeWindow(`Horizontal, focus, -10)
        |> resizeWindow(`Vertical, focus, -10),
        Nothing,
      )
    | None => (model, Nothing)
    }

  | Command(IncreaseSize) =>
    switch (focus) {
    | Some(focus) => (
        model
        |> resizeWindow(`Horizontal, focus, 10)
        |> resizeWindow(`Vertical, focus, 10),
        Nothing,
      )
    | None => (model, Nothing)
    }

  | Command(DecreaseHorizontalSize) =>
    switch (focus) {
    | Some(focus) => (
        model |> resizeWindow(`Horizontal, focus, -10),
        Nothing,
      )
    | None => (model, Nothing)
    }

  | Command(IncreaseHorizontalSize) =>
    switch (focus) {
    | Some(focus) => (
        model |> resizeWindow(`Horizontal, focus, 10),
        Nothing,
      )
    | None => (model, Nothing)
    }

  | Command(DecreaseVerticalSize) =>
    switch (focus) {
    | Some(focus) => (model |> resizeWindow(`Vertical, focus, -10), Nothing)
    | None => (model, Nothing)
    }

  | Command(IncreaseVerticalSize) =>
    switch (focus) {
    | Some(focus) => (model |> resizeWindow(`Vertical, focus, 10), Nothing)
    | None => (model, Nothing)
    }

  | Command(ResetSizes) => (resetWeights(model), Nothing)
  };
};

// VIEW

module View = {
  open Revery;
  open UI;

  module Constants = {
    let handleSize = 10;
  };

  module Styles = {
    open Style;

    let container = [flexGrow(1), flexDirection(`Row)];

    let verticalHandle = (node: Positioned.t(_)) => [
      cursor(MouseCursors.horizontalResize),
      position(`Absolute),
      left(node.meta.x + node.meta.width - Constants.handleSize / 2),
      top(node.meta.y),
      width(Constants.handleSize),
      height(node.meta.height),
    ];

    let horizontalHandle = (node: Positioned.t(_)) => [
      cursor(MouseCursors.verticalResize),
      position(`Absolute),
      left(node.meta.x),
      top(node.meta.y + node.meta.height - Constants.handleSize / 2),
      width(node.meta.width),
      height(Constants.handleSize),
    ];
  };

  let component = React.Expert.component("handleView");
  let handleView = (~direction, ~node: Positioned.t(_), ~onDrag, ()) =>
    component(hooks => {
      let ((captureMouse, _state), hooks) =
        Hooks.mouseCapture(
          ~onMouseMove=
            ((lastX, lastY), evt) => {
              let delta =
                switch (direction) {
                | `Vertical => evt.mouseX -. lastX
                | `Horizontal => evt.mouseY -. lastY
                };

              onDrag(delta);
              Some((evt.mouseX, evt.mouseY));
            },
          ~onMouseUp=(_, _) => None,
          (),
          hooks,
        );

      let onMouseDown = (evt: NodeEvents.mouseButtonEventParams) => {
        captureMouse((evt.mouseX, evt.mouseY));
      };

      (
        <View
          onMouseDown
          style={
            direction == `Vertical
              ? Styles.verticalHandle(node) : Styles.horizontalHandle(node)
          }
        />,
        hooks,
      );
    });

  let rec nodeView =
          (
            ~theme,
            ~path=[],
            ~node: Positioned.t(_),
            ~renderWindow,
            ~dispatch,
            (),
          ) => {
    switch (node.kind) {
    | `Split(direction, children) =>
      let rec loop = (index, children) => {
        let path = [index, ...path];

        switch (children) {
        | [] => []
        | [node] => [<nodeView theme path node renderWindow dispatch />]

        | [node, ...[_, ..._] as rest] =>
          let onDrag = delta => {
            dispatch(
              HandleDragged({
                path: List.rev(path),
                delta: int_of_float(delta),
              }),
            );
          };
          [
            <nodeView theme path node renderWindow dispatch />,
            <handleView direction node onDrag />,
            ...loop(index + 1, rest),
          ];
        };
      };

      loop(0, children) |> React.listToElement;

    | `Window(id) =>
      <View
        style=Style.[
          position(`Absolute),
          left(node.meta.x),
          top(node.meta.y),
          width(node.meta.width),
          height(node.meta.height),
        ]>
        {renderWindow(id)}
      </View>
    };
  };

  let make = (~children as renderWindow, ~model, ~theme, ~dispatch, ()) => {
    let children = {
      let positioned =
        Positioned.fromLayout(0, 0, model.width, model.height, model.tree);

      <nodeView theme node=positioned renderWindow dispatch />;
    };

    <View
      onDimensionsChanged={({width, height}) =>
        dispatch(DimensionsChanged({width, height}))
      }
      style=Styles.container>
      children
    </View>;
  };
};

module Commands = {
  open Feature_Commands.Schema;

  let rotateForward =
    define(
      ~category="View",
      ~title="Rotate Windows (Forwards)",
      "view.rotateForward",
      Command(RotateForward),
    );

  let rotateBackward =
    define(
      ~category="View",
      ~title="Rotate Windows (Backwards)",
      "view.rotateBackward",
      Command(RotateBackward),
    );

  let moveLeft =
    define(
      ~category="View",
      ~title="Move Window Focus Left",
      "window.moveLeft",
      Command(MoveLeft),
    );

  let moveRight =
    define(
      ~category="View",
      ~title="Move Window Focus Right",
      "window.moveRight",
      Command(MoveRight),
    );

  let moveUp =
    define(
      ~category="View",
      ~title="Move Window Focus Up",
      "window.moveUp",
      Command(MoveUp),
    );

  let moveDown =
    define(
      ~category="View",
      ~title="Move Window Focus Down",
      "window.moveDown",
      Command(MoveDown),
    );

  let decreaseSize =
    define(
      ~category="View",
      ~title="Decrease Current Window/View Size",
      "workbench.action.decreaseViewSize",
      Command(DecreaseSize),
    );

  let increaseSize =
    define(
      ~category="View",
      ~title="Increase Current Window/View Size",
      "workbench.action.increaseViewSize",
      Command(IncreaseSize),
    );

  let decreaseHorizontalSize =
    define(
      ~category="View",
      ~title="Decrease Horizontal Window Size",
      "vim.decreaseHorizontalWindowSize",
      Command(DecreaseHorizontalSize),
    );

  let increaseHorizontalSize =
    define(
      ~category="View",
      ~title="Increase Horizontal Window Size",
      "vim.increaseHorizontalWindowSize",
      Command(IncreaseHorizontalSize),
    );

  let decreaseVerticalSize =
    define(
      ~category="View",
      ~title="Decrease Vertical Window Size",
      "vim.decreaseVerticalWindowSize",
      Command(DecreaseVerticalSize),
    );

  let increaseVerticalSize =
    define(
      ~category="View",
      ~title="Increase Vertical Window Size",
      "vim.increaseVerticalWindowSize",
      Command(IncreaseVerticalSize),
    );

  let resetSizes =
    define(
      ~category="View",
      ~title="Reset Window Sizes",
      "workbench.action.evenEditorWidths",
      Command(ResetSizes),
    );
};

module Contributions = {
  let commands =
    Commands.[
      rotateForward,
      rotateBackward,
      moveLeft,
      moveRight,
      moveUp,
      moveDown,
      increaseSize,
      decreaseSize,
      increaseHorizontalSize,
      decreaseHorizontalSize,
      increaseVerticalSize,
      decreaseVerticalSize,
      resetSizes,
    ];
};
