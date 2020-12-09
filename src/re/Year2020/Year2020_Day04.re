open Belt;

let sampleInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm
iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929
hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm
hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in";

let input = Util.readInput(~year=2020, ~day=4)

module Passport = {
  type t = {
    byr: string,
    iyr: string,
    eyr: string,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option(string),
  };

  let make = s => {
    let tokens = Js.String2.split(s, " ");
    let kv =
      tokens
      ->Array.map(token => {
          let res = Js.String2.split(token, ":");
          (res->Array.getUnsafe(0), res->Array.getUnsafe(1));
        })
      ->Map.String.fromArray;

    let%Opt byr = kv->Map.String.get("byr");
    let%Opt iyr = kv->Map.String.get("iyr");
    let%Opt eyr = kv->Map.String.get("eyr");
    let%Opt hgt = kv->Map.String.get("hgt");
    let%Opt hcl = kv->Map.String.get("hcl");
    let%Opt ecl = kv->Map.String.get("ecl");
    let%Opt pid = kv->Map.String.get("pid");
    let cid = kv->Map.String.get("cid");

    Some({byr, iyr, eyr, hgt, hcl, ecl, pid, cid});
  };
};

module ValidPassport = {
  type token =
    | BirthYear(int)
    | IssueYear(int)
    | ExpirationYear(int)
    | Height(height)
    | HairColor(string)
    | EyeColor(string)
    | PassportId(string)
    | CountryId(string)
  and height =
    | In(int)
    | Cm(int);

  type t = {
    byr: token,
    iyr: token,
    eyr: token,
    hgt: token,
    hcl: token,
    ecl: token,
    pid: token,
    cid: option(token),
  };

  let parseByr = s => {
    switch (int_of_string(s)) {
    | v when v >= 1920 && v <= 2002 => Some(BirthYear(v))
    | _ => None
    };
  };

  let parseIyr = s => {
    switch (int_of_string(s)) {
    | v when v >= 2010 && v <= 2020 => Some(IssueYear(v))
    | _ => None
    };
  };

  let parseEyr = s => {
    switch (int_of_string(s)) {
    | v when v >= 2020 && v <= 2030 => Some(IssueYear(v))
    | _ => None
    };
  };

  let parseHgt = s => {
    switch (
      Js.String2.substr(s, ~from=-2),
      Js.String2.substrAtMost(s, ~from=0, ~length=Js.String2.length(s) - 2)
      ->int_of_string_opt,
    ) {
    | ("cm", Some(v)) when v >= 150 && v <= 193 => Some(Height(Cm(v)))
    | ("in", Some(v)) when v >= 59 && v <= 76 => Some(Height(In(v)))
    | _ => None
    };
  };

  let parseHcl = s => {
    let isHex = c => {
      switch (c) {
      | "0"
      | "1"
      | "2"
      | "3"
      | "4"
      | "5"
      | "6"
      | "7"
      | "8"
      | "9"
      | "a"
      | "b"
      | "c"
      | "d"
      | "e"
      | "f" => true
      | _ => false
      };
    };

    Js.String2.length(s) == 7
    && Js.String2.startsWith(s, "#")
    && Js.String2.substr(s, ~from=1)
       ->Garter.String.toArray
       ->Array.every(isHex)
      ? Some(HairColor(s)) : None;
  };

  let parseEcl = s => {
    switch (s) {
    | "amb"
    | "blu"
    | "brn"
    | "gry"
    | "grn"
    | "hzl"
    | "oth" => Some(EyeColor(s))
    | _ => None
    };
  };

  let parsePid = s => {
    let isDigit = c => {
      switch (c) {
      | "0"
      | "1"
      | "2"
      | "3"
      | "4"
      | "5"
      | "6"
      | "7"
      | "8"
      | "9" => true
      | _ => false
      };
    };
    s->Js.String2.length == 9
    && s->Garter.String.toArray->Array.every(isDigit)
      ? Some(PassportId(s)) : None;
  };

  let make = (p: Passport.t) => {
    let%Opt byr = p.byr->parseByr;
    let%Opt iyr = p.iyr->parseIyr;
    let%Opt eyr = p.eyr->parseEyr;
    let%Opt hgt = p.hgt->parseHgt;
    let%Opt hcl = p.hcl->parseHcl;
    let%Opt ecl = p.ecl->parseEcl;
    let%Opt pid = p.pid->parsePid;
    let cid = p.cid->Option.map(x => CountryId(x));
    Some({byr, iyr, eyr, hgt, hcl, ecl, pid, cid});
  };
};

let part1 = input => {
  input->Util.splitLines->Array.keepMap(Passport.make)->Array.length;
};

assert(part1(sampleInput) == 2);
part1(input)->Js.log;

let part2 = input => {
  input
  ->Util.splitLines
  ->Array.keepMap(Passport.make)
  ->Array.keepMap(ValidPassport.make)
  ->Array.length;
};

let invalidSamples = "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946
hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
hgt:59cm ecl:zzz eyr:2038 hcl:74454a iyr:2023 pid:3556412378 byr:2007";

let validSamples = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f
eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022
iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";

assert(part2(invalidSamples) == 0);
assert(part2(validSamples) == 4);
assert(part2(sampleInput) == 2);
part2(input)->Js.log;
