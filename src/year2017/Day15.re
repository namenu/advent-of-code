module type Factor = {let factor: int;};

module Generator = {
  module Make = (F: Factor) => {
    type t = {n: Int64.t};
    let factor = F.factor->Int64.of_int;
    let m = 2147483647->Int64.of_int;

    let make = n0 => {
      {n: n0->Int64.of_int};
    };

    let next = g => {
      {n: g.n->Int64.mul(factor)->Int64.rem(m)};
    };

    let nextMultiple = (g, multiple) => {
      let m = multiple->Int64.of_int;
      let rec f = g => {
        Int64.equal(g.n->Int64.rem(m), Int64.zero) ? g : f(next(g));
      };
      f(next(g));
    };

    let lowest = g => {
      g.n->Int64.to_int land (1 lsl 16 - 1);
    };
  };
};

let bits = n => {
  (n lsr 0)->Js.Int.toStringWithRadix(~radix=2);
};

module GenA =
  Generator.Make({
    let factor = 16807;
  });
module GenB =
  Generator.Make({
    let factor = 48271;
  });

let part1 = () => {
  let rec findMatch = (gA, gB, count, found) => {
    if (count mod 1000000 == 0) {
      Js.log(count);
    };

    if (count > 40000000) {
      found;
    } else {
      let found' = GenA.lowest(gA) == GenB.lowest(gB) ? found + 1 : found;
      findMatch(GenA.next(gA), GenB.next(gB), count + 1, found');
    };
  };

  //assert(findMatch(GenA.make(65), GenB.make(8921), 0, 0) == 588);

  findMatch(GenA.make(618), GenB.make(814), 0, 0)->Js.log;
};

let part2 = () => {
  let rec findMatch = (gA, gB, count, found) => {
    if (count mod 1000000 == 0) {
      Js.log(count);
    };

    if (count > 5000000) {
      found;
    } else {
      let found' = GenA.lowest(gA) == GenB.lowest(gB) ? found + 1 : found;
      findMatch(
        GenA.nextMultiple(gA, 4),
        GenB.nextMultiple(gB, 8),
        count + 1,
        found',
      );
    };
  };

  //assert(findMatch(GenA.make(65), GenB.make(8921), 0, 0) == 309);

  findMatch(GenA.make(618), GenB.make(814), 0, 0)->Js.log;
};

part2();
