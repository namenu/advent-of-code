open Belt;

[@bs.module "./util"] external gets: string => Js.Promise.t(string) = "gets";

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


/*

잘한거:
 - oeis
 - List.keepMap, int_of_string_opt

삽질:
 - 매번 외부의 bs 바인딩에 의존하려는 생각은 버리는 것이 좋을 것 같아요.
 - 그 바인딩이 어떤 라이브러리 버전에 의존했는지 모르고, 그마저도 완전함이 보장되지 않았어요.
 - Node<->BS 바인딩이 맘에 드는게 없었고, 8.2 호환이 되지 않아서 시간만 왕창 버렸습니다.
   - 정작 저에게 필요한 interop 함수는 하나였는데, 직접 만드니 5분도 안걸렸어요...

기타:
 - Belt.List를 확장해서 List.takeExn, List.dropExn 이라는 함수를 추가하고 싶은데 잘 안됐습니다.
   - 지금와서 생각해보니 외부 라이브러리를 임의로 확장하는 것은 좋지 않은거 같아요.
 - async/await 이 없네요. Pipe가 있기 때문인가..?
 - lazy seq를 다뤄보고 싶어요

*/
