open Belt;

let sampleInput = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#";
let input = Node.Fs.readFileAsUtf8Sync("input/year2020/day03.in");

module Area = {
  type t = {
    width: int,
    height: int,
    area: array(array(string)),
  };

  let make = input => {
    let area = input->Util.splitByLines->Array.map(Garter.String.toArray);
    let width = area->Array.getUnsafe(0)->Array.length;
    let height = area->Array.length;

    {width, height, area};
  };

  let slope = ({width, height, area}, (mx, my)) => {
    let length = Js.Math.ceil_int(height->float_of_int /. my->float_of_int);
    let xs = Array.range(0, length - 1)->Array.map(v => v * mx);
    let ys = Array.range(0, length - 1)->Array.map(v => v * my);

    let isTree = (x, y) => {
      area->Array.getUnsafe(y)->Array.getUnsafe(x) === "#";
    };

    Array.zip(xs, ys)
    ->Array.keep(((x, y)) => isTree(x mod width, y))
    ->Array.length;
  };
};

let part1 = input => {
  Area.make(input)->Area.slope((3, 1))->Js.log;
};

let part2 = input => {
  let area = Area.make(input);
  let slopes = [|(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)|];

  slopes->Array.map(Area.slope(area))->Array.reduce(1, ( * ))->Js.log;
};

part2(input);
