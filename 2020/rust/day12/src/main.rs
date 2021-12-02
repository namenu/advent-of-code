/*
F10
N3
F7
R90
F11
*/

use std::str::FromStr;

#[derive(Debug)]
enum Action {
    N,
    S,
    E,
    W,
    L,
    R,
    F,
}

#[derive(Debug)]
struct Nav {
    action: Action,
    value: i32,
}

#[derive(Debug)]
struct NavError;

impl FromStr for Nav {
    type Err = NavError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.chars();
        let action = match it.next().unwrap() {
            'N' => Action::N,
            'S' => Action::S,
            'E' => Action::E,
            'W' => Action::W,
            'L' => Action::L,
            'R' => Action::R,
            'F' | _ => Action::F,
            // TODO: parse error
        };
        let value: i32 = i32::from_str_radix(it.as_str(), 10).unwrap();
        Ok(Nav { action, value })
    }
}

fn main() {
    let n = Nav::from_str("F10").unwrap();
    println!("{:?}", n);

    println!("Hello, world!");
}
