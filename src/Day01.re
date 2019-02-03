let input = Node_fs.readFileSync("input/day01.in", `utf8);

let list_of_digits = input =>
  Js.String.castToArrayLike(input)
  ->Js.Array.fromMap(int_of_string)
  ->List.fromArray;

let rotate = (xs, step) =>
  List.concat(xs, xs)
  ->List.drop(step)
  ->Option.flatMap(List.take(_, List.length(xs)))
  ->Option.getWithDefault([]);

let captcha = (xs, ys) =>
  List.zip(xs, ys)
  ->List.map(((x, y)) => x == y ? x : 0)
  ->List.reduce(0, (+));

let part1 = {
  let xs = list_of_digits(input);
  let ys = rotate(xs, 1)
  captcha(xs, ys)->Js.log
}

let part2 = {
  let xs = list_of_digits(input);
  let ys = rotate(xs, List.length(xs) / 2)
  captcha(xs, ys)->Js.log
}
