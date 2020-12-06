open Belt;

let sampleInput = "flqrgnkx";
let input = "ljoxqyyw";

let hashes = input =>
  Array.range(0, 127)
  ->Array.map(i => {
      let s = input ++ "-" ++ string_of_int(i);
      s->KnotHash.fromString;
    });

let part1 = input => {
  hashes(input)->Array.map(KnotHash.countOnes)->Array.reduce(0, (+));
};

// input->part1->Js.log;

module GridGraph = {
  type coord = (int, int);

  module CoordCmp =
    Id.MakeComparable({
      type t = coord;
      let cmp = Pervasives.compare;
    });

  include Graph.MakeGraph(CoordCmp);

  let (maxX, maxY) = (127, 127);

  let adjacents = ((x, y)) => {
    let ret = [];
    let ret = x > 0 ? List.add(ret, (x - 1, y)) : ret;
    let ret = x < maxX ? List.add(ret, (x + 1, y)) : ret;
    let ret = y > 0 ? List.add(ret, (x, y - 1)) : ret;
    let ret = y < maxY ? List.add(ret, (x, y + 1)) : ret;
    ret->List.toArray;
  };

  let fromGrid = grid => {
    let empty = V.make(~id=(module CoordCmp));
    let ones =
      Belt.Array.reduceWithIndex(grid, empty, (acc, row, y) => {
        row
        ->Js.String2.castToArrayLike
        ->Js.Array2.from
        ->Belt.Array.reduceWithIndex(acc, (acc, ch, x) => {
            ch === "1" ? V.add(acc, (x, y)) : acc
          })
      });

    {
      nodes: ones,
      neighbors: v => {
        let adjs = adjacents(v)->verticesFromArray;
        adjs->V.keep(V.has(ones));
      },
    };
  };
};

let part2 = input => {
  let grid = hashes(input)->Belt.Array.map(KnotHash.toBinaryString);
  GridGraph.fromGrid(grid)->GridGraph.groups->Belt.List.size;
};

part2(sampleInput)->Js.log;
