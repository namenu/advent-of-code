open Belt;

let input = Node.Fs.readFileAsUtf8Sync("resources/year2017/day10.in");

let part1 = () => {
  let seq =
    input->Js.String2.split(",")->Array.map(int_of_string)->List.fromArray;
  let knot = KnotHash.make(256, seq);
  Js.log(Array.getUnsafe(knot, 0) * Array.getUnsafe(knot, 1));
};

let part2 = () => {
  input->KnotHash.fromString->KnotHash.toString->Js.log;
};

part1();
part2();

/**
 * ~named argument 는 특히 숫자 인자들이 연속될 경우에 잘 사용되는 것 같습니다.
 * API 만든 사람들이 헷깔리겠다 싶은 부분은 세심하게 named로 바꾸어 둔 느낌...
 *
 * 대부분의 시간을 Js, Belt 라이브러리 탐색하는데 썼습니다.
 * ClojureScript 경험과 비교해보면, 코어 라이브러리의 높은 완성도가 얼마나 축복인지 깨닫습니다.
 *
 * OCaml/Reason의 for/Range 구간 설계는 잘못된 것 같군요.
 * 0-based 인덱스에서 end를 개구간으로 하지 않은 이유가 궁금.
 */;
