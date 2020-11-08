open Belt;

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

let make = (knotSize, seq): t => {
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

let makeSparse = (input: t): t => {
  let postamble = [|17, 31, 73, 47, 23|];
  let pattern = Js.Array2.concat(input, postamble);

  let round = 64;
  let seq =
    Array.range(0, round - 1)
    ->Array.reduce([||], (acc, _) => Array.concat(acc, pattern));

  make(256, List.fromArray(seq));
};

let makeDense = (knot: t): t => {
  let blocks =
    Array.range(0, 15)
    ->Array.map(i => {
        let offset = i * 16;
        Array.slice(knot, ~offset, ~len=16);
      });

  let xor = Array.reduce(_, 0, (lxor));

  Array.map(blocks, xor);
};

let fromString = (s): t => {
  let toAscii = s =>
    Js.String2.castToArrayLike(s)
    ->Js.Array2.from
    ->Js.Array2.map(c => int_of_float(Js.String2.charCodeAt(c, 0)));

  s->toAscii->makeSparse->makeDense;
};

let toString = (knot: t): string => {
  let toHex = d => {
    let h = Js.Int.toStringWithRadix(d, ~radix=16);
    d < 16 ? "0" ++ h : h;
  };
  knot->Array.joinWith("", toHex);
};

module Garter_String = {
  type t = string;

  let charCode = s => {
    int_of_float(Js.String2.charCodeAt(s, 0));
  };

  [@bs.val] external parseInt: (t, ~radix: int=?) => int = "parseInt";

  [@bs.send] external padStart: (t, int, t) => t = "padStart";
};

let to4Bits = n => {
  n->Js.Int.toStringWithRadix(~radix=2)->Garter_String.padStart(4, "0");
};

let toBinaryString = (knot: t): string => {
  knot
  ->toString
  ->Js.String2.castToArrayLike
  ->Js.Array2.from
  ->Array.joinWith("", h => {
      let n = Garter_String.parseInt(h, ~radix=16);
      to4Bits(n);
    });
};

let countOnes = (knot): int => {
  let rec popcnt = n => {
    n === 0 ? 0 : n mod 2 + popcnt(n / 2);
  };
  knot->Array.map(popcnt)->Array.reduce(0, (+));
};
