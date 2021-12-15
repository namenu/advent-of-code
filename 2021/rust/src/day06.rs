use std::collections::HashMap;

fn parse_input(s: &str) -> Vec<i32> {
    s.split(",").flat_map(|n| n.parse()).collect()
}

fn cost(nums: &Vec<i32>, pos: i32) -> i32 {
    nums.iter().map(|&x| (pos - x).abs()).sum()
}

fn cost2(nums: &Vec<i32>, pos: i32) -> i32 {
    nums.iter()
        .map(|&x| {
            let l = (pos - x).abs();
            l * (l + 1) / 2
        })
        .sum()
}

type CostFn = fn(&Vec<i32>, i32) -> i32;

fn find_min_fuel(nums: &Vec<i32>, f: CostFn) -> i32 {
    let min = *nums.iter().min().unwrap();
    let max = *nums.iter().max().unwrap() + 1;
    (min..max).map(|x| f(&nums, x)).min().unwrap()
}

pub fn part1(s: &str) -> i32 {
    let nums = parse_input(s);

    // TODO: cost 를 closure 로 선언하고 싶음
    find_min_fuel(&nums, cost)
}

pub fn part1_linear(s: &str) -> i32 {
    let nums = parse_input(s);

    let mut h: HashMap<i32, i32> = HashMap::new();
    let mut lhs = 0;
    let mut rhs = 0;
    let mut fuel = 0;

    for &n in &nums {
        let e = h.entry(n).or_insert(0);
        *e += 1;
        fuel += n + 1;
        rhs += 1;
    }

    let max = *nums.iter().max().unwrap();
    let mut min_fuel = fuel;

    for pos in 0..=max {
        if let Some(v) = h.remove(&pos) {
            rhs -= v;
            fuel += lhs - rhs - v;
            lhs += v;
        } else {
            fuel += lhs - rhs;
        }
        min_fuel = min_fuel.min(fuel);
    }

    min_fuel
}

pub fn part2(s: &str) -> i32 {
    let nums = parse_input(s);
    find_min_fuel(&nums, cost2)
}

#[cfg(test)]
mod tests {
    static SAMPLE: &str = "16,1,2,0,4,2,7,1,2,14";
    static INPUT: &str = include_str!("../inputs/06.in");

    #[test]
    fn part1() {
        assert_eq!(super::part1(SAMPLE), 37);
        assert_eq!(super::part1(INPUT), 347509);
    }

    #[test]
    fn part1_linear() {
        assert_eq!(super::part1_linear(SAMPLE), 37);
        assert_eq!(super::part1_linear(INPUT), 347509);
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(SAMPLE), 168);
        assert_eq!(super::part2(INPUT), 98257206);
    }
}
