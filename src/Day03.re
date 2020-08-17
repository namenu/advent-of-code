open Belt;

[@bs.module "./util"] external gets: string => Js.Promise.t(string) = "gets";

let input = "#A141481: Square spiral of sums of selected preceding terms, starting at 1..
#Table of n, a(n) for n = 1..961
1 1
2 1
3 2
4 4
5 5
6 10
7 11
8 23
9 25
10 26
11 54
12 57
13 59
14 122
15 133
16 142
17 147
18 304
19 330
20 351
21 362";

let solve = (cheatSheet, n) => {
  let lines = cheatSheet->String.trim->String.split_on_char('\n', _);

  lines
  //->MyList.takeExn(100) // further lines will cause integer parsing error
  ->List.keepMap(l => {
      switch (String.split_on_char(' ', l)) {
      | [_, num] => int_of_string_opt(num)
      | _ => None
      }
    })
  ->List.getBy(x => x > n);
};

let part2 = () => {
  let n = 312051;

  gets("https://oeis.org/A141481/b141481.txt")
  |> Js.Promise.then_(value => {
       let answer = solve(value, n);
       answer->Js.log;

       Js.Promise.resolve(answer);
     })
  |> Js.Promise.catch(e => {
       Js.log(e);
       Js.Promise.resolve(None);
     });
};

part2();
