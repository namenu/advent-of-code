open Belt;

let sampleInput = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc";
let input = Node.Fs.readFileAsUtf8Sync("input/year2020/day02.in");

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
    Garter.Array.frequencies(
      password->Garter.String.toArray,
      ~id=(module Garter.Id.StringComparable),
    );
  switch (freq->Belt.Map.get(letter)) {
  | Some(f) when f >= low && f <= high => true
  | _ => false
  };
};

let validator2 = (({low, high, letter}, password)) => {
  let s = password->Garter.String.toArray;
  let (a, b) = (s->Array.getUnsafe(low - 1), s->Array.getUnsafe(high - 1));
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
  ->Array.keep(validator2)
  ->Array.length
  ->Js.log;
};
