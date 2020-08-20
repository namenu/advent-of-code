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
