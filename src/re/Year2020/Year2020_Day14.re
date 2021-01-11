open Belt;

module V = Garter.Vector;

let sampleInput = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";
let input = Util.readInput(~year=2020, ~day=14);

type mask = {
  stencil: BigInt.t,
  value: BigInt.t,
};

type op =
  | Mask(mask)
  | Store(int, BigInt.t);

let parseMask = str => {
  open! BigInt;

  let (stencil, value) =
    str
    ->Garter.String.toVector
    ->V.reduce(
        (zero, zero),
        ((s, m), ch) => {
          let s = s lsl one;
          let m = m lsl one;
          switch (ch) {
          | "0" => (s, m)
          | "1" => (s, m lor one)
          | _ => (s lor one, m)
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
  let value = re[2]->Option.map(BigInt.fromString)->Option.getExn;
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
  type memory = Map.Int.t(BigInt.t);
  type t = {
    mask,
    memory,
  };

  let make = () => {
    mask: {
      stencil: BigInt.zero,
      value: BigInt.zero,
    },
    memory: Map.Int.empty,
  };

  let store = ({mask, memory}, addr, value) => {
    open! BigInt;
    let mval = value land mask.stencil lor mask.value;
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
System.(
  {
    let program = input->Util.splitLines->V.fromArray->V.map(parse);
    let state1 = System.make()->run(program);
    state1.memory
    ->Map.Int.valuesToArray
    ->V.fromArray
    ->V.reduce(BigInt.zero, BigInt.(+));
  }
)
->Js.log;
