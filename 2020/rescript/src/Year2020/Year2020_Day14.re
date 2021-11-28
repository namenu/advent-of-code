open Belt;

module V = Garter.Vector;

let sampleInput = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";
let input = Util.readInput(~year=2020, ~day=14);

type mask = {
  stencil: Int64.t,
  value: Int64.t,
};

type op =
  | Mask(mask)
  | Store(int, Int64.t);

let parseMask = str => {
  let (stencil, value) =
    str
    ->Garter.String.toVector
    ->V.reduce(
        (Int64.zero, Int64.zero),
        ((s, m), ch) => {
          let s = s->Int64.shift_left(1);
          let m = m->Int64.shift_left(1);
          switch (ch) {
          | "0" => (s, m)
          | "1" => (s, m->Int64.logor(Int64.one))
          | _ => (s->Int64.logor(Int64.one), m)
          };
        },
      );

  Mask({stencil, value});
};

let parseStore = str => {
  let re =
    str
    ->Js.String2.match([%re "/mem\\[(\\d+)\\] = (\\d+)/"])
    ->Option.getUnsafe;

  let addr = re[1]->Option.flatMap(Int.fromString)->Option.getExn;
  let value = re[2]->Option.flatMap(Int64.of_string_opt)->Option.getExn;
  Store(addr, value);
};

let parse = s => {
  let ass = s->Js.String2.split(" = ")->V.fromArray;

  let lhs = ass->V.getExn(0);
  let rhs = ass->V.getExn(1);
  switch (lhs) {
  | "mask" => parseMask(rhs)
  | _ => parseStore(s)
  };
};

module System = {
  type memory = Map.Int.t(Int64.t);
  type t = {
    mask,
    memory,
  };

  let make = () => {
    mask: {
      stencil: Int64.zero,
      value: Int64.zero,
    },
    memory: Map.Int.empty,
  };

  let store = ({mask, memory}, addr, value) => {
    let mval = value->Int64.logand(mask.stencil)->Int64.logor(mask.value);
    {mask, memory: memory->Map.Int.set(addr, mval)};
  };

  let run = (state0, program) => {
    program->V.reduce(state0, (state, op) => {
      switch (op) {
      | Mask(mask) => {...state, mask}
      | Store(addr, value) => state->store(addr, value)
      }
    });
  };
};

// part1
let part1 = input =>
  System.(
    {
      let program = input->Util.splitLines->V.fromArray->V.map(parse);
      let state1 = System.make()->run(program);
      state1.memory
      ->Map.Int.valuesToArray
      ->V.fromArray
      ->V.reduce(Int64.zero, Int64.add)
      ->Int64.to_float;
    }
  );

assert(part1(sampleInput) == 165.0);
assert(part1(input) == 17765746710228.0);
