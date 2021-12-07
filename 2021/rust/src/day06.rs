use std::fs;

static SAMPLE: &str = "16,1,2,0,4,2,7,1,2,14";

fn parse_input() -> Vec<i64> {
    let s = fs::read_to_string("inputs/06.in").unwrap();
    let s = SAMPLE;
    s.split(",").flat_map(|n| n.parse()).collect()
}

fn cost(nums: &Vec<i64>, pos: i64) -> i64 {
    nums.iter().map(|&x| (pos - x).abs()).sum()
}

fn cost2(nums: &Vec<i64>, pos: i64) -> i64 {
    nums.iter()
        .map(|&x| {
            let l = (pos - x).abs();
            l * (l + 1) / 2
        })
        .sum()
}

pub fn part1() -> i64 {
    let nums = parse_input();

    let min = *nums.iter().min().unwrap();
    let max = *nums.iter().max().unwrap() + 1;

    let v = (min..max).map(|x| cost(&nums, x)).min().unwrap();
    println!("{:?}", v);
    
    v
}

pub fn part2() -> i64 {
    let lines = parse_input();
    
    let nums = parse_input();
    
    let min = *nums.iter().min().unwrap();
    let max = *nums.iter().max().unwrap() + 1;
    
    let v = (min..max).map(|x| cost2(&nums, x)).min().unwrap();
    println!("{:?}", v);

    v
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        assert_eq!(super::part1(), 347509)
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(), 98257206)
    }
}
