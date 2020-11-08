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
  type t = array(array(int));

  let (maxX, maxY) = (127, 127);

  let make = () => {
    [|[||]|];
  };

  let adjacents = ((x, y)) => {
    let ret = [];
    let ret = x > 0 ? List.add(ret, (x - 1, y)) : ret;
    let ret = x < maxX ? List.add(ret, (x + 1, y)) : ret;
    let ret = y > 0 ? List.add(ret, (x, y - 1)) : ret;
    let ret = y < maxY ? List.add(ret, (x, y + 1)) : ret;
    ret->List.toArray;
  };

  let countRegions = (g: t): int => {
    0;
  };
};

module CoordCmp =
  Id.MakeComparable({
    type t = (int, int);
    let cmp = Pervasives.compare;
  });

(sampleInput ++ "-0")->KnotHash.fromString->KnotHash.toBinaryString->Js.log;

Grid.adjacents((0, 1))
->Set.fromArray(~id=(module CoordCmp))
->Set.toArray
->Js.log;

// Grid.adjacents((0, 1))->Js.log;
