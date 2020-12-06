open Belt;

module Maze = {
  type t = {
    offsets: array(int),
    pos: int,
  };

  let make = offsets => {
    {offsets: Array.copy(offsets), pos: 0};
  };

  let step = (offsetFn, {offsets, pos}) => {
    let o = Array.getUnsafe(offsets, pos);
    Array.setUnsafe(offsets, pos, offsetFn(o));
    {offsets, pos: pos + o};
  };

  let isOutOfRange = ({offsets, pos}) => {
    pos < 0 || pos >= Array.length(offsets);
  };

  let run = (m, ~offsetFn=o => o + 1, ()): int => {
    let stepFn = step(offsetFn);
    let rec run' = (m, count) =>
      if (isOutOfRange(m)) {
        count;
      } else {
        stepFn(m)->run'(count + 1);
      };
    run'(m, 0);
  };
};

let input =
  Node_fs.readFileAsUtf8Sync("input/day05.in")
  ->Js.String2.trim
  ->Js.String2.split("\n")
  ->Array.map(int_of_string);
//let input = [|0, 3, 0, 1, (-3)|];

let part1 = () => {
  Maze.make(input)->Maze.run()->Js.log;
};

let part2 = () => {
  let offsetFn = o => o >= 3 ? o - 1 : o + 1;
  Maze.make(input)->Maze.run(~offsetFn, ())->Js.log;
};

part1();
part2();
