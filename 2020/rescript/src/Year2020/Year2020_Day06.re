open Belt;

let sampleInput = "abc\n\na\nb\nc\n\n\nab\nac\n\na\na\na\na\n\nb\n";
let input = Util.readInput(~year=2020, ~day=6);

module GroupAnswers = {
  module Answers = Set.String;

  type t = array(Answers.t);

  let make = (xs: array(string)): t =>
    xs->Array.map(x => x->Garter.String.toArray->Answers.fromArray);

  let everyAnswers = Garter.Array.reduce1(_, Answers.union);

  let commonAnswers = Garter.Array.reduce1(_, Answers.intersect);

  let sum = (xs: t): int =>
    xs->Array.map(Answers.size)->Array.reduce(0, (+));
};

let part1 = input => {
  input
  ->Util.splitParagraphs
  ->Array.map(Util.splitLines)
  ->Array.map(p => p->GroupAnswers.make->GroupAnswers.everyAnswers)
  ->GroupAnswers.sum
  ->Js.log;
};

sampleInput->part1;
input->part1;

let part2 = input => {
  input
  ->Util.splitParagraphs
  ->Array.map(Util.splitLines)
  ->Array.map(p => p->GroupAnswers.make->GroupAnswers.commonAnswers)
  ->GroupAnswers.sum
  ->Js.log;
};

sampleInput->part2;
input->part2;
