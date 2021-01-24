open Garter;

let sampleInput = "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576";
let input = Util.readInput(~year=2020, ~day=09);

let isInvalidChunk = ((chunk, target)) => {
  chunk
  ->Vector.fromArray
  ->Year2020_Day01.findPairs(~sum=target)
  ->Vector.toArray
  ->Array.isEmpty;
};

let part1 = (input, preamble) => {
  let data = input->Util.splitLines->Array.map(float_of_string);

  let chunks = data->Array.windows(~n=preamble, ~step=1, ());
  let targets = data->Array.sliceToEnd(preamble);

  Array.zip(chunks, targets)
  ->Array.keep(isInvalidChunk)
  ->Array.getUnsafe(0)
  ->snd;
};

assert(part1(sampleInput, 5) == 127.0);
assert(part1(input, 25) == 144381670.0);

type state = {
  range: (int, int),
  sum: float,
};

let findWeakness = (data, target) => {
  let next = ({range, sum} as state) => {
    let (i, j) = range;
    if (sum == target) {
      Ok(state);
    } else if (state.sum < target) {
      Error({sum: sum +. data->Array.getUnsafe(j), range: (i, j + 1)});
    } else {
      Error({sum: sum -. data->Array.getUnsafe(i), range: (i + 1, j)});
    };
  };
  let rec iter = state => {
    switch (next(state)) {
    | Ok(state') => state'
    | Error(state') => iter(state')
    };
  };
  iter({range: (0, 0), sum: 0.0});
};

let part2 = (input, target) => {
  let data = input->Util.splitLines->Array.map(float_of_string);
  let (i, j) = data->findWeakness(target).range;

  let set =
    Array.slice(data, ~offset=i, ~len=j - i)
    ->Belt.Set.fromArray(~id=(module Id.FloatComparable));

  BsBastet.Option.Infix.(
    Some((+.)) <*> set->Belt.Set.minimum <*> set->Belt.Set.maximum
  )
  ->Belt.Option.getUnsafe;
};

assert(part2(sampleInput, 127.0) == 62.0);
assert(part2(input, 144381670.0) == 20532569.0);
