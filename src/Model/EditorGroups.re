/*
 * EditorGroups.re
 *
 * Managing an aggregate of EditorGroups
 */

open Oni_Core;

type t = {
  idToGroup: IntMap.t(EditorGroup.t),
  activeId: int,
  /* Cache the last editor font, so when a new group is created, we can share it */
  lastEditorFont: option(Service_Font.font),
};

let create = () => {
  let defaultGroup = EditorGroup.create();
  let activeId = defaultGroup.editorGroupId;
  let idToGroup = IntMap.add(activeId, defaultGroup, IntMap.empty);

  {idToGroup, activeId, lastEditorFont: None};
};

let activeGroupId = model => model.activeId;

let getById = (model, id) => IntMap.find_opt(id, model.idToGroup);

let getActive = model => getById(model, model.activeId);

let setActive = (id, model) => {...model, activeId: id};

let isActive = (model, group: EditorGroup.t) =>
  group.editorGroupId == model.activeId;

let applyToAll = (~defaultFont, editors, action: Actions.t) =>
  IntMap.map(
    group => EditorGroupReducer.reduce(~defaultFont, group, action),
    editors,
  );

let setBufferFont = (~bufferId, ~font, groups) => {
  let idToGroup =
    groups.idToGroup
    |> IntMap.map(group => EditorGroup.setBufferFont(~bufferId, ~font, group));

  {...groups, idToGroup};
};

/* Validate 'activeId' is set to a valid editor group,
   otherwise move to the first valid */
let ensureActiveId = model => {
  switch (IntMap.find_opt(model.activeId, model.idToGroup)) {
  | Some(_) => model
  | None =>
    switch (IntMap.min_binding_opt(model.idToGroup)) {
    | Some((key, _)) => {...model, activeId: key}
    | _ => model
    }
  };
};

let isEmpty = (id, model) => {
  switch (IntMap.find_opt(id, model.idToGroup)) {
  | None => true
  | Some(group) => EditorGroup.isEmpty(group)
  };
};

let removeEmpty = model => {
  let idToGroup =
    IntMap.filter(
      (_, group) => !EditorGroup.isEmpty(group),
      model.idToGroup,
    );

  {...model, idToGroup};
};

let reduce = (~defaultFont, model, action: Actions.t) => {
  switch (action) {
  | EditorFont(Service_Font.FontLoaded(font)) => {
      ...model,
      idToGroup: applyToAll(~defaultFont, model.idToGroup, action),
      lastEditorFont: Some(font),
    }
  | EditorSizeChanged(_) => {
      ...model,
      idToGroup: applyToAll(~defaultFont, model.idToGroup, action),
    }

  | EditorGroupAdd(editorGroup) =>
    let editorGroup =
      switch (model.lastEditorFont) {
      | Some(font) =>
        EditorGroupReducer.reduce(
          ~defaultFont,
          editorGroup,
          EditorFont(Service_Font.FontLoaded(font)),
        )
      | None => editorGroup
      };

    {
      ...model,
      activeId: editorGroup.editorGroupId,
      idToGroup:
        IntMap.add(editorGroup.editorGroupId, editorGroup, model.idToGroup),
    };

  | action =>
    let newModel =
      switch (getActive(model)) {
      | Some(group) => {
          ...model,
          idToGroup:
            IntMap.add(
              model.activeId,
              EditorGroupReducer.reduce(~defaultFont, group, action),
              model.idToGroup,
            ),
        }
      | None => model
      };

    switch (action) {
    | ViewCloseEditor(_) => newModel |> removeEmpty |> ensureActiveId
    | _ => newModel
    };
  };
};
