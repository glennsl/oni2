include AbstractTree;

[@deriving show({with_path: false})]
type metadata = {
  size: float,
  minWidth: float,
  minHeight: float,
};

[@deriving show({with_path: false})]
type t('id) = AbstractTree.node('id, metadata);

// DSL

module DSL = {
  open AbstractTree.DSL;

  let split = (~size=1., direction, children) => {
    let metadata =
      switch (direction) {
      | `Vertical => {
          size,
          minWidth:
            children
            |> List.map(c => c.meta.minWidth)
            |> List.fold_left((+.), 0.),
          minHeight:
            children
            |> List.map(c => c.meta.minHeight)
            |> List.fold_left(max, 0.),
        }
      | `Horizontal => {
          size,
          minWidth:
            children
            |> List.map(c => c.meta.minWidth)
            |> List.fold_left(max, 0.),
          minHeight:
            children
            |> List.map(c => c.meta.minHeight)
            |> List.fold_left((+.), 0.),
        }
      };

    split(metadata, direction, children);
  };
  let vsplit = (~size=1., children) => split(~size, `Vertical, children);
  let hsplit = (~size=1., children) => split(~size, `Horizontal, children);
  let window = (~minWidth=100, ~minHeight=100, ~size=1., id) =>
    window(
      {size, minWidth: float(minWidth), minHeight: float(minHeight)},
      id,
    );

  let withSize = (size, node) => node |> withMetadata({...node.meta, size});
};

include DSL;

let empty = vsplit([]);

let singleton = id => vsplit([window(id)]);

/**
 * addWindow
 */
let addWindow = (insertDirection, idToInsert, tree) => {
  switch (tree.kind) {
  | `Split(_, []) => window(~size=tree.meta.size, idToInsert)

  | `Split(direction, [firstChild, ...remainingChildren])
      when direction != insertDirection =>
    split(
      ~size=tree.meta.size,
      direction,
      [
        split(insertDirection, [window(idToInsert), firstChild]),
        ...remainingChildren,
      ],
    )

  | `Split(direction, children) =>
    split(
      ~size=tree.meta.size,
      direction,
      [window(idToInsert), ...children],
    )

  | `Window(id) =>
    split(
      ~size=tree.meta.size,
      insertDirection,
      [window(idToInsert), window(id)],
    )
  };
};

let%test_module "addWindow" =
  (module
   {
     let%test "add vertical split" = {
       let actual = empty |> addWindow(`Vertical, 1);

       actual == window(1);
     };

     let%test "parent split changes direction if needed" = {
       let actual =
         hsplit([window(2), window(1)]) |> addWindow(`Vertical, 3);

       actual == hsplit([vsplit([window(3), window(2)]), window(1)]);
     };
   });

/**
 * insertWindow
 */
let insertWindow = (position, insertDirection, idToInsert, tree) => {
  let `Before(targetId) | `After(targetId) = position;

  let splitWindow = node =>
    split(
      ~size=node.meta.size,
      insertDirection,
      switch (position) {
      | `Before(_) => [window(idToInsert), node |> withSize(1.)]
      | `After(_) => [node |> withSize(1.), window(idToInsert)]
      },
    );

  let rec traverse = node =>
    switch (node.kind) {
    | `Split(_, []) => {...node, kind: `Window(idToInsert)} // HACK: to work around this being intially called with an idea that doesn't yet exist in the tree
    | `Split(direction, children) when direction == insertDirection =>
      let onMatch = child =>
        switch (position) {
        | `Before(_) => [window(idToInsert), child]
        | `After(_) => [child, window(idToInsert)]
        };
      split(
        ~size=node.meta.size,
        direction,
        traverseChildren(~onMatch, [], children),
      );

    | `Split(direction, children) =>
      let onMatch = node => [splitWindow(node)];
      split(
        ~size=node.meta.size,
        direction,
        traverseChildren(~onMatch, [], children),
      );

    | `Window(id) when id == targetId => splitWindow(node)

    | `Window(_) => node
    }

  and traverseChildren = (~onMatch, before, after) =>
    switch (after) {
    | [] => List.rev(before)
    | [child, ...rest] =>
      switch (child.kind) {
      | `Window(id) when id == targetId =>
        traverseChildren(~onMatch, List.rev(onMatch(child)) @ before, rest)

      | `Window(_) => traverseChildren(~onMatch, [child, ...before], rest)

      | `Split(_) =>
        traverseChildren(~onMatch, [traverse(child), ...before], rest)
      }
    };

  traverse(tree);
};

let%test_module "insertWindow" =
  (module
   {
     let%test "insert vertical split" = {
       let actual = window(1) |> insertWindow(`Before(1), `Vertical, 2);

       actual == vsplit([window(2), window(1)]);
     };

     let%test "insert vertical split - after" = {
       let actual = window(1) |> insertWindow(`After(1), `Vertical, 2);

       actual == vsplit([window(1), window(2)]);
     };

     let%test "parent split changes direction if needed" = {
       let actual =
         hsplit([window(2), window(1)])
         |> insertWindow(`Before(1), `Vertical, 3);

       actual == hsplit([window(2), vsplit([window(3), window(1)])]);
     };
   });

/**
 * removeWindow
 */
let removeWindow = (target, tree) => {
  let rec traverse = node =>
    switch (node.kind) {
    | `Split(direction, children) =>
      switch (List.filter_map(traverse, children)) {
      | [] => None
      // BUG: Collapsing disabled as it doesn't preserve size properly.
      // | [child] => Some(child)
      | newChildren =>
        Some(split(~size=node.meta.size, direction, newChildren))
      }
    | `Window(id) when id == target => None
    | `Window(_) => Some(node)
    };

  traverse(tree) |> Option.value(~default=empty);
};

let%test_module "removeWindow" =
  (module
   {
     let%test "nested - remove 4" = {
       let initial =
         vsplit([
           hsplit([window(3), window(2)]),
           hsplit([window(4), window(1)]),
         ]);

       let actual = initial |> removeWindow(4);

       actual
       == vsplit([hsplit([window(3), window(2)]), hsplit([window(1)])]);
     };
     let%test "nested - remove 3" = {
       let initial =
         vsplit([hsplit([window(3), window(2)]), hsplit([window(1)])]);

       let actual = initial |> removeWindow(3);

       actual == vsplit([hsplit([window(2)]), hsplit([window(1)])]);
     };
     let%test "nested - remove 2 - empty parent split removed" = {
       let initial = vsplit([hsplit([window(2)]), hsplit([window(1)])]);

       let actual = initial |> removeWindow(2);

       actual == vsplit([hsplit([window(1)])]);
     };
   });

// INTERNAL
open {
       let totalWeight = nodes =>
         nodes
         |> List.map(child => child.meta.size)
         |> List.fold_left((+.), 0.)
         |> max(1.);

       /**
        * shiftWeightRight
        *
        * Shifts weight from `nodes[index]` to `nodes[index+1]` if delta is positive,
        * vice versa if negative. The amount of weight shifted depends on the space
        * available as specified by `available`, the total weight of all nodes, and
        * the minimum weight of each node.
        */
       let shiftWeightRight = (~available, ~delta, index, nodes) => {
         let unitSize = float(available) /. totalWeight(nodes);

         let rec loop = i =>
           fun
           | [] => []
           | [node] => [node]
           | [node, next, ...rest] when i == index => {
               let delta = float(delta);
               let weight = node.meta.size;
               let size = weight *. unitSize;
               let nextWeight = next.meta.size;
               let nextSize = nextWeight *. unitSize;

               let delta =
                 if (size +. delta < node.meta.minWidth) {
                   min(0., -. (size -. node.meta.minWidth));
                 } else if (nextSize -. delta < next.meta.minWidth) {
                   max(0., nextSize -. next.meta.minWidth);
                 } else {
                   delta;
                 };

               let deltaWeight = delta /. unitSize;

               [
                 node |> withSize(weight +. deltaWeight),
                 next |> withSize(nextWeight -. deltaWeight),
                 ...rest,
               ];
             }
           | [node, ...rest] => [node, ...loop(i + 1, rest)];

         loop(0, nodes);
       };

       let%test_module "shiftWeightRight" =
         (module
          {
            let sizes = (~available, nodes) => {
              open Base.Float;

              let unitSize = of_int(available) / totalWeight(nodes);

              nodes
              |> List.map(node => node.meta.size * unitSize |> round |> to_int);
            };

            let%test "sanity check: sizes - even" =
              sizes(~available=300, [window(1), window(2), window(3)])
              == [100, 100, 100];
            let%test "sanity check: sizes - uneven" =
              sizes(
                ~available=300,
                [window(~size=0.95, 1), window(~size=1.05, 2), window(3)],
              )
              == [95, 105, 100];

            let%test "positive delta" = {
              let available = 600;
              let initial = [window(1), window(2), window(3)];

              let actual =
                initial |> shiftWeightRight(~available, ~delta=5, 1);

              sizes(~available, actual) == [200, 205, 195];
            };

            let%test "negative delta" = {
              let available = 600;
              let initial = [window(1), window(2), window(3)];

              let actual =
                initial |> shiftWeightRight(~available, ~delta=-5, 1);

              sizes(~available, actual) == [200, 195, 205];
            };

            let%test "inital at minimum" = {
              let available = 300;
              let initial = [window(1), window(2), window(3)];

              let actual =
                initial |> shiftWeightRight(~available, ~delta=5, 1);

              sizes(~available, actual) == [100, 100, 100];
            };

            let%test "initial below minimum" = {
              let available = 150;
              let initial = [window(1), window(2), window(3)];

              let actual =
                initial |> shiftWeightRight(~available, ~delta=5, 1);

              sizes(~available, actual) == [50, 50, 50];
            };

            let%test "initial below minimum - negative delta" = {
              let available = 150;
              let initial = [window(1), window(2), window(3)];

              let actual =
                initial |> shiftWeightRight(~available, ~delta=-5, 1);

              sizes(~available, actual) == [50, 50, 50];
            };

            let%test "delta > available" = {
              let available = 600;
              let initial = [window(1), window(2), window(3)];

              let actual =
                initial |> shiftWeightRight(~available, ~delta=700, 1);

              sizes(~available, actual) == [200, 300, 100];
            };
          });

       let shiftWeightDown = (~available, ~delta, index, nodes) => {
         let unitSize = float(available) /. totalWeight(nodes);

         let rec loop = i =>
           fun
           | [] => []
           | [node] => [node]
           | [node, next, ...rest] when i == index => {
               let delta = float(delta);
               let weight = node.meta.size;
               let size = weight *. unitSize;
               let nextWeight = next.meta.size;
               let nextSize = nextWeight *. unitSize;

               let delta =
                 if (size +. delta < node.meta.minHeight) {
                   min(0., -. (size -. node.meta.minHeight));
                 } else if (nextSize -. delta < next.meta.minHeight) {
                   max(0., nextSize -. next.meta.minHeight);
                 } else {
                   delta;
                 };

               let deltaWeight = delta /. unitSize;

               [
                 node |> withSize(weight +. deltaWeight),
                 next |> withSize(nextWeight -. deltaWeight),
                 ...rest,
               ];
             }
           | [node, ...rest] => [node, ...loop(i + 1, rest)];

         loop(0, nodes);
       };
     };

/**
 * resizeSplit
 */
let rec resizeSplit = (~width, ~height, ~path, ~delta, node) => {
  switch (path) {
  | [] => node // shouldn't happen

  | [index] =>
    switch (node.kind) {
    | `Split(`Vertical, children) =>
      vsplit(
        ~size=node.meta.size,
        shiftWeightRight(~available=width, ~delta, index, children),
      )

    | `Split(`Horizontal, children) =>
      hsplit(
        ~size=node.meta.size,
        shiftWeightDown(~available=height, ~delta, index, children),
      )

    | `Window(_) => node
    }

  | [index, ...rest] =>
    switch (node.kind) {
    | `Split(direction, children) =>
      let available = direction == `Vertical ? width : height;
      let unitSize = float(available) /. totalWeight(children);

      split(
        ~size=node.meta.size,
        direction,
        List.mapi(
          (i, child) => {
            let size = int_of_float(unitSize *. child.meta.size);
            let (width, height) =
              switch (direction) {
              | `Vertical => (size, height)
              | `Horizontal => (width, size)
              };

            i == index
              ? resizeSplit(~width, ~height, ~path=rest, ~delta, child)
              : child;
          },
          children,
        ),
      );
    | `Window(_) => node
    }
  };
};

let%test_module "resizeSplit" =
  (module
   {
     let%test "vsplit" = {
       let width = 300;
       let height = 300;
       let initial = vsplit([window(1), window(2)]);

       let actual =
         initial |> resizeSplit(~width, ~height, ~path=[0], ~delta=30);

       actual == vsplit([window(~size=1.2, 1), window(~size=0.8, 2)]);
     };

     let%test "hsplit" = {
       let width = 300;
       let height = 300;
       let initial = hsplit([window(1), window(2)]);

       let actual =
         initial |> resizeSplit(~width, ~height, ~path=[0], ~delta=30);

       actual == hsplit([window(~size=1.2, 1), window(~size=0.8, 2)]);
     };

     let%test "vsplit + hsplit" = {
       let width = 300;
       let height = 300;
       let initial = vsplit([window(3), hsplit([window(1), window(2)])]);

       let actual =
         initial |> resizeSplit(~width, ~height, ~path=[1, 0], ~delta=30);

       actual
       == vsplit([
            window(3),
            hsplit([window(~size=1.2, 1), window(~size=0.8, 2)]),
          ]);
     };

     let%test "hsplit + vsplit" = {
       let width = 300;
       let height = 300;
       let initial = hsplit([window(3), vsplit([window(1), window(2)])]);

       let actual =
         initial |> resizeSplit(~width, ~height, ~path=[1, 0], ~delta=30);

       actual
       == hsplit([
            window(3),
            vsplit([window(~size=1.2, 1), window(~size=0.8, 2)]),
          ]);
     };

     let%test "vsplit + vsplit - at minimum" = {
       let width = 300;
       let height = 300;
       let initial = vsplit([window(3), vsplit([window(1), window(2)])]);

       let actual =
         initial |> resizeSplit(~width, ~height, ~path=[1, 0], ~delta=30);

       actual == vsplit([window(3), vsplit([window(1), window(2)])]);
     };

     let%test "vsplit + vsplit - above minimum" = {
       let width = 600;
       let height = 600;
       let initial = vsplit([window(3), vsplit([window(1), window(2)])]);

       let actual =
         initial |> resizeSplit(~width, ~height, ~path=[1, 0], ~delta=30);

       actual
       == vsplit([
            window(3),
            vsplit([window(~size=1.2, 1), window(~size=0.8, 2)]),
          ]);
     };
   });

/**
 * resizeWindow
 */
let resizeWindow = (~width, ~height, direction, targetId, delta, node) => {
  let delta = float(delta);

  let rec traverse = (~width, ~height, ~parentDirection=?, ~unitSize=1., node) =>
    switch (node.kind) {
    | `Split(dir, children) =>
      let available = dir == `Vertical ? width : height;
      let unitSize = float(available) /. totalWeight(children);

      let (result, children) =
        List.fold_left(
          ((accResult, accChildren), child) => {
            let size = int_of_float(unitSize *. child.meta.size);
            let (width, height) =
              switch (direction) {
              | `Vertical => (size, height)
              | `Horizontal => (width, size)
              };

            let (result, newChild) =
              traverse(
                ~width,
                ~height,
                ~parentDirection=dir,
                ~unitSize,
                child,
              );

            (
              result == `NotFound ? accResult : result,
              [newChild, ...accChildren],
            );
          },
          (`NotFound, []),
          List.rev(children),
        );

      let deltaWeight = delta /. unitSize;

      switch (result, parentDirection) {
      | (`NotAdjusted, Some(parentDirection))
          when parentDirection != direction => (
          `Adjusted,
          split(~size=node.meta.size *. deltaWeight, dir, children),
        )

      | _ => (result, split(~size=node.meta.size, dir, children))
      };

    | `Window(id) when id == targetId =>
      if (parentDirection == Some(direction)) {
        (`NotAdjusted, node);
      } else {
        let deltaWeight = delta /. unitSize;

        (`Adjusted, node |> withSize(node.meta.size *. deltaWeight));
      }

    | `Window(_) => (`NotFound, node)
    };

  traverse(~width, ~height, node) |> snd;
};

let%test_module "resizeWindow" =
  (module
   {
     let resizeWindow = resizeWindow(~width=600, ~height=600);

     let%test "vsplit  - vresize" = {
       let initial = vsplit([window(1), window(2)]);

       let actual = resizeWindow(`Vertical, 2, 5, initial);

       Console.log(show(Fmt.int, actual));
       actual == vsplit([window(1), window(2)]);
     };

     let%test "vsplit  - hresize" = {
       let initial = vsplit([window(1), window(2)]);

       let actual = resizeWindow(`Horizontal, 2, 5, initial);

       actual == vsplit([window(1), window(~size=5., 2)]);
     };

     let%test "hsplit  - hresize" = {
       let initial = hsplit([window(1), window(2)]);

       let actual = resizeWindow(`Horizontal, 2, 5, initial);

       actual == hsplit([window(1), window(2)]);
     };

     let%test "hsplit  - vresize" = {
       let initial = hsplit([window(1), window(2)]);

       let actual = resizeWindow(`Vertical, 2, 5, initial);

       actual == hsplit([window(1), window(~size=5., 2)]);
     };

     let%test "vsplit+hsplit - hresize" = {
       let initial = vsplit([window(1), hsplit([window(2), window(3)])]);

       let actual = resizeWindow(`Horizontal, 2, 5, initial);

       actual
       == vsplit([window(1), hsplit(~size=5., [window(2), window(3)])]);
     };

     let%test "vsplit+hsplit - vresize" = {
       let initial = vsplit([window(1), hsplit([window(2), window(3)])]);

       let actual = resizeWindow(`Vertical, 2, 5, initial);

       actual
       == vsplit([window(1), hsplit([window(~size=5., 2), window(3)])]);
     };

     let%test "hsplit+vsplit - hresize" = {
       let initial = hsplit([window(1), vsplit([window(2), window(3)])]);

       let actual = resizeWindow(`Horizontal, 2, 5, initial);

       actual
       == hsplit([window(1), vsplit([window(~size=5., 2), window(3)])]);
     };

     let%test "hsplit+vsplit - vresize" = {
       let initial = hsplit([window(1), vsplit([window(2), window(3)])]);

       let actual = resizeWindow(`Vertical, 2, 5, initial);

       actual
       == hsplit([window(1), vsplit(~size=5., [window(2), window(3)])]);
     };
   });

/**
 * resetWeights
 */
let resetWeights = tree => AbstractTree.map(withSize(1.), tree);
