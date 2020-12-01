open Belt;

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
  let ys = rotate(xs, 1);
  captcha(xs, ys)->Js.log;
};

let part2 = {
  let xs = list_of_digits(input);
  let ys = rotate(xs, List.length(xs) / 2);
  captcha(xs, ys)->Js.log;
};


/**
 * 잘못:
 *  - 시퀀스 자료구조를 활용한 코드가 나왔는데, ReasonML 코어 개발자들은 linked list를 권장하지 않는 듯 하네요.
 *  - https://reasonml.chat/t/bucklescript-8-1-new-syntax-arrays-and-lists/2394/27
 *  - 실제로 시퀀스 자료구조에 유용한 함수들도 별로 없고요.
 *
 * 기타:
 *  - Maybe 모나드 대신 Option 모듈을 사용한게 일반적인가봐요.
 *
 */
