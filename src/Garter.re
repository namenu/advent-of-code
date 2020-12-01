module List = {
  include Belt.List;

  let takeExn = (list, cnt) => {
    switch (Belt.List.take(list, cnt)) {
    | Some(l) => l
    | None => raise(Not_found)
    };
  };

  let dropExn = (list, cnt) => {
    switch (Belt.List.drop(list, cnt)) {
    | Some(l) => l
    | None => raise(Not_found)
    };
  };

  let rec orderedPairs = xs => {
    switch (xs) {
    | [] => []
    | [x, ...ys] => Belt.List.map(ys, y => (x, y)) @ orderedPairs(ys)
    };
  };
};

module Array = {
  let isEmpty = ar => Belt.Array.size(ar) === 0;

  /** reduce와 비슷하나 중간 결과를 모두 포함한 array를 반환해줌 */
  let scan = (xs, init, f) => {
    open Belt.Array;
    let state = makeUninitializedUnsafe(length(xs));
    let cur = ref(init);
    Belt.Array.forEachWithIndex(
      xs,
      (idx, x) => {
        cur := f(cur^, x);
        setUnsafe(state, idx, cur^);
      },
    );
    state;
  };

  let max = xs => {
    let res = Belt.Array.getUnsafe(xs, 0);
    Belt.Array.reduce(xs, res, (x, res) => max(x, res));
  };

  /** Returns (max_value, index). Array may not be empty. */
  let maxIndex = xs => {
    let init = (Belt.Array.getUnsafe(xs, 0), 0);
    Belt.Array.reduceWithIndex(
      xs,
      init,
      (acc, v, idx) => {
        let (curMax, curIdx) = acc;
        compare(v, curMax) > 0 ? (v, idx) : (curMax, curIdx);
      },
    )
    ->snd;
  };

  let updateUnsafe = (ar, i, f) => {
    let v = Belt.Array.getUnsafe(ar, i);
    Belt.Array.setUnsafe(ar, i, f(v));
  };

  include Belt.Array;
};

module String = {
  type t = string;

  let charCode = s => {
    int_of_float(Js.String2.charCodeAt(s, 0));
  };

  [@bs.val] external parseInt: (t, ~radix: int=?) => int = "parseInt";

  [@bs.send] external padStart: (t, int, t) => t = "padStart";
};

module Pair = {
  let first = ((first, _)) => first;
  let second = ((_, second)) => second;
};
