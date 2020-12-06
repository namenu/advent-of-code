open Belt;

module HexGrid = {
  /**
   * Axial coordinate
   * x + y + z = 0
   */
  type t = {
    x: int,
    y: int,
  };

  type dirs =
    | NW
    | N
    | NE
    | SW
    | S
    | SE;

  let origin = {x: 0, y: 0};

  let move = ({x, y}, dir) => {
    switch (dir) {
    | NW => {x, y: y + 1}
    | N => {x: x + 1, y: y + 1}
    | NE => {x: x + 1, y}
    | SW => {x: x - 1, y}
    | S => {x: x - 1, y: y - 1}
    | SE => {x, y: y - 1}
    };
  };

  let rec moveMany = (pos, moves) => {
    switch (moves) {
    | [] => pos
    | [m, ...moves] => move(pos, m)->moveMany(moves)
    };
  };

  let distanceFromOrigin = ({x, y}) => {
    let z = x - y;
    Js.Math.(abs_int(x) + abs_int(y) + abs_int(z)) / 2;
  };
};

let parse = text => {
  HexGrid.(
    text
    ->Js.String2.trim
    ->Js.String2.split(",")
    ->Array.map(x =>
        switch (x) {
        | "nw" => NW
        | "n" => N
        | "ne" => NE
        | "sw" => SW
        | "s" => S
        | "se" => SE
        | _ => raise(Not_found)
        }
      )
  );
};

let part1 = input => {
  let moves = parse(input)->List.fromArray;
  HexGrid.origin->HexGrid.moveMany(moves)->HexGrid.distanceFromOrigin;
};

assert(part1("ne,ne,ne") == 3);
assert(part1("ne,ne,sw,sw") == 0);
assert(part1("ne,ne,s,s") == 2);
assert(part1("se,sw,se,sw,sw") == 3);

let input = Node.Fs.readFileAsUtf8Sync("input/day11.in");
part1(input)->Js.log;

let part2 = input => {
  let moves = parse(input);

  Garter.Array.scan(moves, HexGrid.origin, HexGrid.move)
  ->Array.map(HexGrid.distanceFromOrigin)
  ->Js.Math.maxMany_int;
};

part2(input)->Js.log;
