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
};

module Array = {
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
