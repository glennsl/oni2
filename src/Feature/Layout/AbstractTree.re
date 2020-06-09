[@deriving show({with_path: false})]
type node('id, 'meta) = {
  meta: 'meta,
  kind: [
    | `Split([ | `Horizontal | `Vertical], Pvec.t(node('id, 'meta)))
    | `Window('id)
  ],
};

module DSL = {
  let split = (meta, direction, children) => {
    meta,
    kind: `Split((direction, children |> Pvec.of_list)),
  };
  let vsplit = (meta, children) => split(meta, `Vertical, children);
  let hsplit = (meta, children) => split(meta, `Horizontal, children);
  let window = (meta, id) => {meta, kind: `Window(id)};

  let withMetadata = (meta, node) => {...node, meta};
  let withChildren = (children, node) =>
    switch (node.kind) {
    | `Split(direction, _) => {...node, kind: `Split(direction, children) }
    | `Window(_) => node
    };
};

let rec fold = (f, acc, node) =>
  switch (node.kind) {
  | `Window(_) => f(node, acc)
  | `Split(_, children) => Pvec.fold_left(fold(f), acc, children)
  };

let rec map = (f, node) =>
  switch (node.kind) {
  | `Window(_) => f(node)
  | `Split(direction, children) =>
    f({...node, kind: `Split((direction, Pvec.map(map(f), children)))})
  };

let rec windowNodes = node =>
  switch (node.kind) {
  | `Window(_) => Pvec.singleton(node)
  | `Split(_, children) => children |> Pvec.map(windowNodes) |> Pvec.concat
  };

let rec windows = node =>
  switch (node.kind) {
  | `Window(id) => Pvec.singleton(id)
  | `Split(_, children) => children |> Pvec.map(windows) |> Pvec.concat
  };

let path = (targetId, node) => {
  exception Found(list(int));
  
  let rec traverse = (path, node) =>
    switch (node.kind) {
    | `Split(_, children) => 
      Pvec.iteri_left(i => traverse([i, ...path]), children)
    | `Window(id) when id == targetId => raise(Found(List.rev(path)))
    | `Window(_) => ()
    };

  switch (traverse([], node)) {
  | () => None
  | exception Found(path) => Some(path)
  };
};

let rec rotate = (direction, targetId, node) => {
  switch (node.kind) {
   |`Split(dir, children) =>
    let children =
      if (Pvec.exists(child => child.kind == `Window(targetId), children)) {
        let len = Pvec.length(children);
        switch (direction) {
        | `Backward => Pvec.init(~len, i => Pvec.get(children, (i - 1) mod len))
        | `Forward => Pvec.init(~len, i => Pvec.get(children, (i + 1) mod len))
        };
      } else {
        Pvec.map(rotate(direction, targetId), children);
      };

    {...node, kind: `Split((dir, children))};

  | `Window(_) => node
  };
};

let%test_module "rotate" =
  (module
   {
     module DSL = {
       include DSL;
       let hsplit = hsplit();
       let vsplit = vsplit();
       let window = window();
     };
     open DSL;

     let%test "forward - simple tree" = {
       let initial = vsplit([window(3), window(2), window(1)]);

       let actual = rotate(`Forward, 3, initial);

       actual == vsplit([window(1), window(3), window(2)]);
     };

     let%test "forward - nested tree" = {
       let initial =
         vsplit([window(4), hsplit([window(3), window(2), window(1)])]);

       let actual = rotate(`Forward, 1, initial);

       actual
       == vsplit([window(4), hsplit([window(1), window(3), window(2)])]);
     };

     let%test "backward - simple tree" = {
       let initial = vsplit([window(3), window(2), window(1)]);

       let actual = rotate(`Backward, 3, initial);

       actual == vsplit([window(2), window(1), window(3)]);
     };

     let%test "backward - nested tree" = {
       let initial =
         vsplit([window(4), hsplit([window(3), window(2), window(1)])]);

       let actual = rotate(`Backward, 1, initial);

       actual
       == vsplit([window(4), hsplit([window(2), window(1), window(3)])]);
     };
   });
