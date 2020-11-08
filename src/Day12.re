let sampleInput = "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5";
let input = Node_fs.readFileAsUtf8Sync("input/day12.in");

let parse = s => {
  let%Opt res = [%re "/(\\d+) <-> (.*)/"]->Js.Re.exec_(s);
  let matches = Js.Re.captures(res)->Belt.Array.map(Js.Nullable.toOption);
  let%Opt lhs = matches[1];
  let%Opt rhs = matches[2];

  let neighbors = rhs->Js.String2.split(", ")->Belt.Array.map(int_of_string);

  Some((int_of_string(lhs), neighbors));
};

module Graph = {
  module V = Belt.Set.Int;
  module E = Belt.Map.Int;

  type t = E.t(array(int));
  type vertex = int;
  type vertices = V.t;

  let fromInput = (input): t => {
    input
    ->Js.String2.trim
    ->Js.String2.split("\n")
    ->Belt.Array.keepMap(parse)
    ->E.fromArray;
  };

  let nodes = g => g->E.keysToArray->V.fromArray;

  let neighbors = (g, v) => E.getExn(g, v)->V.fromArray;

  let dfs = (g, startNode): vertices => {
    let rec visit = (curNode, visited: vertices) => {
      g
      ->neighbors(curNode)
      ->V.reduce(V.add(visited, curNode), (visited, nextNode) => {
          V.has(visited, nextNode) ? visited : visit(nextNode, visited)
        });
    };

    visit(startNode, V.empty);
  };

  let groups = (g): list(vertices) => {
    let rec visitAll = (unvisited: vertices): list(vertices) => {
      switch (unvisited->V.toList) {
      | [] => []
      | [u, ..._] =>
        let visited = dfs(g, u);
        [visited, ...visitAll(unvisited->V.removeMany(visited->V.toArray))];
      };
    };

    let unvisited = g->nodes;
    visitAll(unvisited);
  };
};

let part1 = () => {
  Graph.fromInput(input)->Graph.dfs(0)->Graph.V.size->Js.log;
};

let part2 = () => {
  Graph.fromInput(input)->Graph.groups->Belt.List.size->Js.log;
};

part1();
part2();
