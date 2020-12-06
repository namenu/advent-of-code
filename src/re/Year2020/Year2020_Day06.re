open Belt;

let sampleInput = "abc

a
b
c

ab
ac

a
a
a
a

b
";

let input = Node.Fs.readFileAsUtf8Sync("resources/year2020/day06.in");

module Answers = Set.String;

module GroupAnswers = {
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
  ->Array.map(p => p->GroupAnswers.make->GroupAnswers.everyAnswers)
  ->GroupAnswers.sum
  ->Js.log;
};

sampleInput->part1;
input->part1;

let part2 = input => {
  input
  ->Util.splitParagraphs
  ->Array.map(p => p->GroupAnswers.make->GroupAnswers.commonAnswers)
  ->GroupAnswers.sum
  ->Js.log;
};

sampleInput->part2;
input->part2;
