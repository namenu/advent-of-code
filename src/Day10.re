open Belt;

module KnotHash = {
  type t = array(int);

  let reverseSub = (ar, offset, limit) => {
    let n = Array.length(ar);
    let swapIndices = (i, j) => {
      let x = Array.getUnsafe(ar, i);
      let y = Array.getUnsafe(ar, j);
      Array.setUnsafe(ar, i, y);
      Array.setUnsafe(ar, j, x);
    };

    Range.forEach(0, limit / 2 - 1, i =>
      swapIndices((offset + i) mod n, (offset + limit - i - 1) mod n)
    );
    ar;
  };

  let make = (knotSize, seq) => {
    let knot = Array.range(0, knotSize - 1);

    let rec pinch = (knot, seq, offset, skipSize) => {
      switch (seq) {
      | [] => knot
      | [x, ...xs] =>
        reverseSub(knot, offset, x)
        ->pinch(xs, (offset + x + skipSize) mod knotSize, skipSize + 1)
      };
    };

    pinch(knot, seq, 0, 0);
  };

  let makeSparse = input => {
    let postamble = [|17, 31, 73, 47, 23|];
    let pattern = Js.Array2.concat(input, postamble);

    let round = 64;
    let seq =
      Array.range(0, round - 1)
      ->Array.reduce([||], (acc, _) => Array.concat(acc, pattern));

    make(256, List.fromArray(seq));
  };

  let makeDense = knot => {
    let blocks =
      Array.range(0, 15)
      ->Array.map(i => {
          let offset = i * 16;
          Array.slice(knot, ~offset, ~len=16);
        });

    let xor = Array.reduce(_, 0, (lxor));

    Array.map(blocks, xor);
  };
};

let input = Node.Fs.readFileAsUtf8Sync("input/day10.in");

let part1 = () => {
  let seq =
    input->Js.String2.split(",")->Array.map(int_of_string)->List.fromArray;
  let knot = KnotHash.make(256, seq);
  Js.log(Array.getUnsafe(knot, 0) * Array.getUnsafe(knot, 1));
};

let part2 = () => {
  let toAscii = s =>
    Js.String2.castToArrayLike(s)
    ->Js.Array2.from
    ->Js.Array2.map(c => int_of_float(Js.String2.charCodeAt(c, 0)));

  let fromAscii = s => {
    let toHex = d => {
      let h = Js.Int.toStringWithRadix(d, ~radix=16);
      d < 16 ? "0" ++ h : h;
    };
    s->Array.map(toHex)->Js.String2.concatMany("", _);
  };

  input->toAscii->KnotHash.makeSparse->KnotHash.makeDense->fromAscii->Js.log;
};

part1();
part2();
