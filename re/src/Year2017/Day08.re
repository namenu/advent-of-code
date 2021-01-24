open Belt;

module Instruction = {
  type t = {
    reg: string,
    v: int,
    condReg: string,
    condOp: (int, int) => bool,
    condV: int,
  };

  let fromString = s => {
    let ar = Js.String2.split(s, " ");

    let parseOp = o =>
      switch (o) {
      | "<" => (<)
      | ">" => (>)
      | "<=" => (<=)
      | ">=" => (>=)
      | "==" => (==)
      | "!=" => (!=)
      | _ => raise(Not_found)
      };

    let sign = Array.getUnsafe(ar, 1) == "inc" ? 1 : (-1);

    {
      reg: Array.getUnsafe(ar, 0),
      v: Array.getUnsafe(ar, 2)->int_of_string * sign,
      condReg: Array.getUnsafe(ar, 4),
      condOp: parseOp(Array.getUnsafe(ar, 5)),
      condV: Array.getUnsafe(ar, 6)->int_of_string,
    };
  };
};

module Cpu = {
  type t = {registers: Map.String.t(int)};

  let make = () => {registers: Map.String.empty};

  let get = (cpu, reg) => cpu.registers->Map.String.getWithDefault(reg, 0);
  let acc = (cpu, reg, v) => {
    registers:
      cpu.registers
      ->Map.String.update(reg, prev => {
          switch (prev) {
          | None => Some(v)
          | Some(u) => Some(u + v)
          }
        }),
  };

  let exec = (cpu, inst: Instruction.t) => {
    let lhs = get(cpu, inst.condReg);
    if (inst.condOp(lhs, inst.condV)) {
      acc(cpu, inst.reg, inst.v);
    } else {
      cpu;
    };
  };

  let getMaximum = cpu =>
    Map.String.valuesToArray(cpu.registers)->Js.Math.maxMany_int;
};

let input = Node_fs.readFileAsUtf8Sync("resources/year2017/day08.in");
let insts = input->Js.String2.split("\n")->Array.map(Instruction.fromString);

let part1 = () => {
  Array.reduce(insts, Cpu.make(), Cpu.exec)->Cpu.getMaximum->Js.log;
};

let part2 = () => {
  let rec findHighest = (cpu, insts) => {
    switch (insts) {
    | [] => Cpu.getMaximum(cpu)
    | [inst, ...insts] =>
      max(Cpu.getMaximum(cpu), findHighest(Cpu.exec(cpu, inst), insts))
    };
  };

  findHighest(Cpu.make(), List.fromArray(insts))->Js.log;
};

part1();
part2();
