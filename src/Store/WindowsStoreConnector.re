/*
 * WindowStoreConnector.re
 *
 * This implements an updater (reducer + side effects) for window management
 */

module Core = Oni_Core;

open Oni_Model;
open Actions;

let setActive = (windowId, editorGroupId, state: State.t) =>
  {
    ...state,
    windowManager: {
      ...state.windowManager,
      activeWindowId: windowId,
    },
    editorGroups: EditorGroups.setActive(editorGroupId, state.editorGroups),
  }
  |> FocusManager.push(Editor);

let move = (moveFunc, state: State.t) => {
  let windowId = moveFunc(state.windowManager);
  let maybeEditorGroupId =
    WindowTree.getEditorGroupIdFromSplitId(
      windowId,
      state.windowManager.windowTree,
    );

  switch (maybeEditorGroupId) {
  | Some(editorGroupId) => setActive(windowId, editorGroupId, state)
  | None => state
  };
};

let start = () => {
  let quitEffect =
    Isolinear.Effect.createWithDispatch(~name="windows.quitEffect", dispatch =>
      dispatch(Actions.Quit(false))
    );

  let initializeDefaultViewEffect = (state: State.t) =>
    Isolinear.Effect.createWithDispatch(~name="windows.init", dispatch => {
      let editor =
        WindowTree.createSplit(
          ~editorGroupId=EditorGroups.activeGroupId(state.editorGroups),
          (),
        );

      dispatch(Actions.AddSplit(Vertical, editor));
    });

  let windowUpdater = (state: State.t, action: Actions.t) =>
    switch (action) {
    | WindowSetActive(splitId, editorGroupId) =>
      setActive(splitId, editorGroupId, state)

    | WindowTreeSetSize(width, height) => {
        ...state,
        windowManager:
          WindowManager.setTreeSize(width, height, state.windowManager),
      }

    | WindowMoveLeft => move(WindowManager.moveLeft, state)

    | WindowMoveRight => move(WindowManager.moveRight, state)

    | WindowMoveUp => move(WindowManager.moveUp, state)

    | WindowMoveDown => move(WindowManager.moveDown, state)

    | AddSplit(direction, split) => {
        ...state,
        // Fix #686: If we're adding a split, we should turn off zen mode... unless it's the first split being added.
        zenMode:
          state.zenMode
          && List.length(
               WindowTree.getSplits(state.windowManager.windowTree),
             )
          == 0,
        windowManager: {
          ...state.windowManager,
          activeWindowId: split.id,
          windowTree:
            WindowTree.addSplit(
              ~target=Some(state.windowManager.activeWindowId),
              ~position=After,
              direction,
              split,
              state.windowManager.windowTree,
            ),
        },
      }

    | RemoveSplit(id) => {
        ...state,
        zenMode: false,
        windowManager: {
          ...state.windowManager,
          windowTree:
            WindowTree.removeSplit(id, state.windowManager.windowTree),
        },
      }

    | ViewCloseEditor(_) =>
      /* When an editor is closed... lets see if any window splits are empty */

      /* Remove splits */
      let windowTree =
        state.windowManager.windowTree
        |> WindowTree.getSplits
        |> List.filter((split: WindowTree.split) =>
             EditorGroups.isEmpty(split.editorGroupId, state.editorGroups)
           )
        |> List.fold_left(
             (prev: WindowTree.t, curr: WindowTree.split) =>
               WindowTree.removeSplit(curr.id, prev),
             state.windowManager.windowTree,
           );

      let windowManager =
        WindowManager.ensureActive({...state.windowManager, windowTree});

      {...state, windowManager};

    | OpenFileByPath(_) => FocusManager.push(Editor, state)

    | WindowRotateForward
    | Command("view.rotateForward") => {
        ...state,
        windowManager: {
          ...state.windowManager,
          windowTree:
            WindowTree.rotateForward(
              state.windowManager.activeWindowId,
              state.windowManager.windowTree,
            ),
        },
      }

    | WindowRotateBackward
    | Command("view.rotateBackward") => {
        ...state,
        windowManager: {
          ...state.windowManager,
          windowTree:
            WindowTree.rotateBackward(
              state.windowManager.activeWindowId,
              state.windowManager.windowTree,
            ),
        },
      }

    | _ => state
    };

  let updater = (state: State.t, action: Actions.t) => {
    let state = windowUpdater(state, action);

    let effect =
      switch (action) {
      | Init => initializeDefaultViewEffect(state)
      // When opening a file, ensure that the active editor is getting focus
      | ViewCloseEditor(_) =>
        if (List.length(WindowTree.getSplits(state.windowManager.windowTree))
            == 0) {
          quitEffect;
        } else {
          Isolinear.Effect.none;
        }
      | _ => Isolinear.Effect.none
      };

    (state, effect);
  };

  updater;
};
