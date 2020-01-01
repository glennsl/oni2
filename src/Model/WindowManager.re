open Revery_UI;

module WindowSplitId =
  Revery.UniqueId.Make({});

module WindowId = {
  let _current = ref(0);
  let current = () => _current^;
  let next = () => {
    incr(_current);
    current();
  };
};

[@deriving show({with_path: false})]
type componentCreator = unit => React.element(React.node);

[@deriving show({with_path: false})]
type t = {
  windowTree: WindowTree.t,
  activeWindowId: int,
  windowTreeWidth: int,
  windowTreeHeight: int,
};

let initialWindowId = WindowId.next();

let create = () => {
  activeWindowId: initialWindowId,
  windowTree: WindowTree.empty,
  windowTreeWidth: 1,
  windowTreeHeight: 1,
};

let setTreeSize = (width, height, state) => {
  ...state,
  windowTreeWidth: width,
  windowTreeHeight: height,
};

/* Ensure the activeWindowId points to a valid winodw */
let ensureActive = state => {
  let splits: list(WindowTree.split) =
    WindowTree.getSplits(state.windowTree);
  let activeWindowId: int = state.activeWindowId;

  let splitIsActive =
    List.exists(
      (split: WindowTree.split) => split.id == activeWindowId,
      splits,
    );

  if (!splitIsActive && List.length(splits) > 0) {
    {...state, activeWindowId: List.hd(splits).id};
  } else {
    state;
  };
};

let move = (dirX, dirY, state) => {
  let layout = WindowTreeLayout.layout(0, 0, 200, 200, state.windowTree);
  let newWindow =
    WindowTreeLayout.move(state.activeWindowId, dirX, dirY, layout);

  switch (newWindow) {
  | None => state.activeWindowId
  | Some(newId) => newId
  };
};

let moveLeft = move(-1, 0);
let moveRight = move(1, 0);
let moveUp = move(0, -1);
let moveDown = move(0, 1);
