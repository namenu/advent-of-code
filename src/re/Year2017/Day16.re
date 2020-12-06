type dance =
  | Spin(int)
  | Exchange(int, int)
  | Partner(string, string);

type program = (string, int);

module Programs = {
  type t = array(program);

  let make = ps => {
    let n = Belt.Array.length(ps);
    Belt.Array.zip(ps, Belt.Array.range(0, n - 1));
  };

  let move = (t, d) => {
    let n = Belt.Array.size(t);
    switch (d) {
    | Spin(s) => Belt.Array.map(t, ((p, idx)) => (p, (idx + s) mod n))
    | Exchange(a, b) =>
      Belt.Array.map(t, ((p, idx)) => {
        switch (idx) {
        | _ when idx === a => (p, b)
        | _ when idx === b => (p, a)
        | _ => (p, idx)
        }
      })
    | Partner(a, b) =>
      Belt.Array.map(t, ((p, idx)) => {
        switch (p) {
        | _ when p === a => (b, idx)
        | _ when p === b => (a, idx)
        | _ => (p, idx)
        }
      })
    };
  };

  let moves = (t, ds) => {
    ds->Belt.Array.reduce(t, move);
  };

  let toString = t => {
    Belt.SortArray.stableSortBy(t, ((_, i), (_, j)) => i - j)
    ->Belt.Array.map(Garter.Pair.first)
    ->Js.Array2.joinWith("");
  };

  // let mapping = (p: t, q: t) => {
  //   Belt.Array.zip(p, q)
  //   ->Belt.Array.map((((_, i), (_, j))) => (i, j))
  //   ->Belt.Map.Int.fromArray
  //   ->Belt.Map.Int.getExn;
  // };

  // let remap = (t, mapping: int => int): t => {
  //   Belt.Array.map(t, ((p, idx)) => {(p, mapping(idx))});
  // };
};

[@warning "-8"]
let parse = s => {
  let body = Js.String2.sliceToEnd(s, ~from=1);

  switch (Js.String2.get(s, 0)) {
  | "s" => Spin(int_of_string(body))
  | "x" =>
    let [|a, b|] = body->Js.String2.split("/");
    Exchange(int_of_string(a), int_of_string(b));
  | "p" =>
    [@warning "-8"]
    let [|a, b|] = body->Js.String2.split("/");
    Partner(a, b);
  };
};

let input = Node_fs.readFileAsUtf8Sync("resources/year2017/day16.in")->Js.String2.trim;
let sampleInput = "s1,x3/4,pe/b";

let part1 = (s, input) => {
  let p = s->Js.String2.castToArrayLike->Js.Array.from->Programs.make;
  let ds = input->Js.String2.split(",")->Belt.Array.map(parse);

  Programs.moves(p, ds)->Programs.toString;
};

// part1("abcde", sampleInput);
// part1("abcdefghijklmnop", input);

let part2 = (s, input, ~repeat) => {
  let p = s->Js.String2.castToArrayLike->Js.Array.from->Programs.make;
  let ds = input->Js.String2.split(",")->Belt.Array.map(parse);

  // let mapping = Programs.mapping(p, Programs.moves(p, ds));

  let rec doMap = (pp, i) => {
    if (pp->Programs.toString === s) {
      // loop length
      Js.log(i);
    }

    if (i === repeat) {
      pp;
    } else {
      doMap(Programs.moves(pp, ds), i + 1);
    };
  };

  doMap(p, 0)->Programs.toString;
};

let repeat = 1000000000;
// let repeat = 10;

part2("abcdefghijklmnop", input, ~repeat)->Js.log;
