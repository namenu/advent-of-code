open Garter;

module XMAS = {
  type t = {
    q: Queue.t(float),
    rest: list(float),
  };

  let make = (data, ~preamble) => {
    let (header, body) = List.splitAt(data, preamble);
    let init = {q: Queue.empty, rest: body};

    header->List.reduce(init, ({q} as state, v) =>
      {...state, q: Queue.snoc(q, v)}
    );
  };
};

let sampleInput = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576";
let input = Util.readInput(~year=2020, ~day=09);

let isInvalidChunk = ((chunk, target)) => {
  Year2020_Day01.findPairs(chunk, ~sum=target)->Array.isEmpty;
};

let part1 = (input, preamble) => {
  let data = input->Util.splitLines->Array.map(float_of_string);

  let chunks = data->Garter.Array.windows(~n=preamble, ~step=1, ());
  let targets = data->Array.sliceToEnd(preamble);

  Array.zip(chunks, targets)
  ->Array.keep(isInvalidChunk)
  ->Array.getUnsafe(0)
  ->Garter.Pair.second
  ->Js.log;
};

// part1(sampleInput, 5);
part1(input, 25);
