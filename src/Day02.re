open Belt;

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

/**
 * early return을 위해 reduce 대신 pattern matching을 써봤습니다.
 * 근데 역시나 List 자료형이라 Array<->List 를 해야 했고, 문제가 있는 코드라는 생각입니다.
 *
 * 아직까지 제가 type을 정의하지 않았는데, 이게 과거 습관 때문인 것 같습니다.
 * 앞으로는 다른 사람들의 하스켈 코드를 좀 참고해서 Type Driven Development를 해보려고요.
 */
