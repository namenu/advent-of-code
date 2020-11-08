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
  module IntCmp =
    Belt.Id.MakeComparable({
      type t = int;
      let cmp = Pervasives.compare;
    });
  include Graph.MakeGraph(IntCmp);

  module E = Belt.Map.Int;

  type edges = E.t(array(int));

  let fromInput = input => {
    let edges =
      input
      ->Js.String2.trim
      ->Js.String2.split("\n")
      ->Belt.Array.keepMap(parse)
      ->E.fromArray;

    {
      nodes: edges->E.keysToArray->verticesFromArray,
      neighbors: v => E.getExn(edges, v)->verticesFromArray,
    };
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
