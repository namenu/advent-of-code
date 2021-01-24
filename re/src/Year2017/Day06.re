open Belt;

let input =
  Node_fs.readFileAsUtf8Sync("resources/year2017/day06.in")
  ->Js.String2.trim
  ->Js.String2.split("\t")
  ->Array.map(Pervasives.int_of_string);

let distributeUnsafe = banks => {
  let i = Garter.Array.maxIndex(banks);
  let length = Array.length(banks);
  let blocks = Array.getUnsafe(banks, i);
  Array.setUnsafe(banks, i, 0);
  let rec f = (blocks, j) =>
    if (blocks > 0) {
      Garter.Array.updateUnsafe(banks, j, (+)(1));
      f(blocks - 1, (j + 1) mod length);
    } else {
      banks;
    };
  f(blocks, (i + 1) mod length);
};

// [|0, 2, 7, 0|]->distributeUnsafe->Js.log; // => (2, 4)

module IntArrayCmp =
  Id.MakeComparable({
    type t = array(int);
    let cmp = Pervasives.compare;
  });

let findDupe = s => {
  let rec go = (history, idx) => {
    let v = Stream.next(s);
    switch (history->Map.get(v)) {
    | None => go(history->Map.set(v, idx), idx + 1)
    | Some(prevIdx) => Some((prevIdx, idx))
    };
  };

  go(Map.make(~id=(module IntArrayCmp)), 0);
};

// [[|0|], [|2|], [|3|], [|1|], [|3|], [|4|]]
// ->Stream.of_list
// ->findDupe
// ->Js.log;

let bankStream = init => {
  let state = ref(init);
  let next = _ => {
    let state' = Array.copy(state^);
    state := distributeUnsafe(state^);
    Some(state');
  };
  Stream.from(next);
};

// bankStream([|0, 2, 7, 0|])->Stream.npeek(10, _)->List.toArray->Js.log;

let (from, to_) = bankStream(input)->findDupe->Option.getExn;

let part1 = () => Js.log(to_);
let part2 = () => Js.log(to_ - from);

/**
 * Stream은 SICP의 스트림이 아니라 Java 스트림에 가까움! (뮤터블)
 * Stream에 들어가는 데이터 역시 Mutable Array다 보니까 주의가 필요합니다. npeek 결과가 모두 동일해버리는 실수라던가...
 * Map은 항상 Make를 해서 사용해야 하는데, 매번 펑터에 들어갈 모듈을 만드는게 번거로워요.
 */;
