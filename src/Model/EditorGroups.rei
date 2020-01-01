type t;

let create: unit => t;

let activeGroupId: t => int; // TODO: Should return an option, or be removed entirely
let getById: (t, int) => option(EditorGroup.t);
let getActive: t => option(EditorGroup.t);
let setActive: (int, t) => t;

let isActive: (t, EditorGroup.t) => bool;
let isEmpty: (int, t) => bool;

let reduce: (~defaultFont: Service_Font.font, t, Actions.t) => t;

let setBufferFont: (~bufferId: int, ~font: Service_Font.font, t) => t;
