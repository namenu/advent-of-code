open Belt;

let sampleInput = "flqrgnkx";
let input = "ljoxqyyw";

let part1 = input => {
  let hashes =
    Array.range(0, 127)
    ->Array.map(i => {
        let s = input ++ "-" ++ string_of_int(i);
        s->KnotHash.fromString;
      });

  hashes->Array.map(KnotHash.countOnes)->Array.reduce(0, (+));
};

// input->part1->Js.log;

module Grid = {
  type coord = (int, int);

  module CoordCmp =
    Id.MakeComparable({
      type t = coord;
      let cmp = Pervasives.compare;
    });

  module GridGraph = Graph.MakeGraph(CoordCmp);

  let (maxX, maxY) = (127, 127);

  let toVertices = GridGraph.V.fromArray(_, ~id=(module CoordCmp));

  let adjacents = ((x, y)) => {
    let ret = [];
    let ret = x > 0 ? List.add(ret, (x - 1, y)) : ret;
    let ret = x < maxX ? List.add(ret, (x + 1, y)) : ret;
    let ret = y > 0 ? List.add(ret, (x, y - 1)) : ret;
    let ret = y < maxY ? List.add(ret, (x, y + 1)) : ret;
    ret->List.toArray->toVertices;
  };

  type t = GridGraph.vertices;

  let make = (): t => {
    [|(1, 0), (1, 1), (1, 2), (3, 0)|]->toVertices;
  };

  let makeGraph = (grid: t) => {
    GridGraph.{
      nodes: grid,
      neighbors: v => adjacents(v)->GridGraph.V.keep(GridGraph.V.has(grid)),
    };
  };

  let countRegions = g => {
    g->GridGraph.groups->Belt.List.size;
  };
};

Grid.make()->Grid.makeGraph->Grid.countRegions->Js.log

// Comparable => Graph

// Grid.adjacents((0, 1))->Js.log;
