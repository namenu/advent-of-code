open Belt;

module Program = {
  type t =
    | Internal(string, int)
    | External(string, int, array(string));

  let fromString = s => {
    let headBody = Js.String2.split(s, " -> ");

    // pares head

    let head = Array.getUnsafe(headBody, 0);
    let (name, weight) = {
      let r = [%re "/(\w+) \((\d+)\)/"]->Js.Re.exec_(head)->Option.getExn;
      let matches = Js.Re.captures(r)->Array.map(Js.Nullable.toOption);
      (
        Js.Array2.unsafe_get(matches, 1)->Option.getExn,
        Js.Array2.unsafe_get(matches, 2)->Option.getExn->int_of_string,
      );
    };

    switch (headBody[1]) {
    | None => Internal(name, weight)
    | Some(body) =>
      let children = Js.String2.split(body, ", ");
      External(name, weight, children);
    };
  };
};

let programs =
  Node_fs.readFileAsUtf8Sync("input/day07.in")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(Program.fromString);

/** Map<child, parent> */
let buildParentMap = programs => {
  programs->Array.reduce(Map.String.empty, (m, p) => {
    switch (p) {
    | Program.Internal(_) => m
    | Program.External(parent, _, children) =>
      children->Array.reduce(m, (m, child) =>
        Map.String.set(m, child, parent)
      )
    }
  });
};

let rec findRoot = (tree, node) => {
  switch (Map.String.get(tree, node)) {
  | None => node
  | Some(parent) => findRoot(tree, parent)
  };
};

let part1 = () => {
  buildParentMap(programs)->findRoot("llyhqfe")->Js.log;
};
