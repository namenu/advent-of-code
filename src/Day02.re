let input = {
  let lines =
    Node_fs.readFileAsUtf8Sync("input/day02.in")
    ->Js.String2.trim
    ->Js.String2.split("\n");
  let parseInts = line =>
    line->Js.String2.split("\t")->Array.map(int_of_string);

  lines->Array.map(parseInts);
};

let part1 = () => {
  Array.(
    input
    ->map(row => reduce(row, min_int, max) - reduce(row, max_int, min))
    ->reduce(0, (+))
  );
};

let part2 = () => {
  let intQuot = (xs, x) =>
    xs
    ->List.getBy(y => x mod y == 0 || y mod x == 0)
    ->Option.map(d => max(d, x) / min(d, x));

  let rec division = xs =>
    switch (xs) {
    | [] => 0
    | [x, ...ys] =>
      switch (intQuot(ys, x)) {
      | None => division(ys)
      | Some(q) => q
      }
    };

  input
  ->Array.map(xs => division(List.fromArray(xs)))
  ->Array.reduce(0, (+));
};

part1()->Js.log;
part2()->Js.log;
