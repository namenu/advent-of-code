open Garter;

let sampleInput = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc";
let input = Util.readInput(~year=2020, ~day=2);

type policy = {
  low: int,
  high: int,
  letter: string,
};

let parse = line => {
  let%Opt res = Js.Re.exec_([%re "/(\\d+)-(\\d+) (\\w+): (\\w+)/"], line);
  let matches = Js.Re.captures(res)->Array.map(Js.Nullable.toOption);

  let%Opt low = matches->Array.getUnsafe(1);
  let%Opt high = matches->Array.getUnsafe(2);
  let%Opt letter = matches->Array.getUnsafe(3);
  let%Opt password = matches->Array.getUnsafe(4);

  Some((
    {low: low->int_of_string, high: high->int_of_string, letter},
    password,
  ));
};

let validator1 = (({low, high, letter}, password)) => {
  let freq =
    password
    ->String.toVector
    ->Vector.frequencies(~id=(module Id.StringComparable));
  switch (freq->Belt.Map.get(letter)) {
  | Some(f) when f >= low && f <= high => true
  | _ => false
  };
};

let validator2 = (({low, high, letter}, password)) => {
  let s = password->String.toVector;
  let (a, b) = (s->Vector.getExn(low - 1), s->Vector.getExn(high - 1));
  switch (a == letter, b == letter) {
  | (true, false)
  | (false, true) => true
  | _ => false
  };
};

let part2 = input => {
  input
  ->Util.splitLines
  ->Array.keepMap(parse)
  ->Vector.fromArray
  ->Vector.keep(validator2)
  ->Vector.length;
};

assert(part2(sampleInput) == 1);
assert(part2(input) == 588);
