enum Direction {
    Forward,
    Down,
    Up,
}

struct Command {
    dir: Direction,
    value: i32,
}

static SAMPLE: &str = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";

fn parse_input() -> Vec<Command> {
    let _input = SAMPLE;
    let input = std::fs::read_to_string("inputs/02.in").unwrap();
    input
        .lines()
        .map(|l| {
            let mut words = l.split(' ');
            let dir = match words.next().unwrap() {
                "forward" => Direction::Forward,
                "down" => Direction::Down,
                "up" => Direction::Up,
                _ => panic!(),
            };
            let value: i32 = words.next().unwrap().parse().unwrap();
            Command { dir, value }
        })
        .collect()
}

pub fn part1() -> i32 {
    let (mut x, mut y) = (0, 0);
    for Command { dir, value } in parse_input() {
        match dir {
            Direction::Forward => x += value,
            Direction::Down => y += value,
            Direction::Up => y -= value,
        }
    }
    x * y
}

pub fn part2() -> i32 {
    let (mut x, mut y) = (0, 0);
    let mut aim = 0;
    for Command { dir, value } in parse_input() {
        match dir {
            Direction::Down => aim += value,
            Direction::Up => aim -= value,
            Direction::Forward => {
                x += value;
                y += aim * value;
            }
        }
    }
    x * y
}

#[cfg(test)]
mod tests {
    #[test]
    fn part1() {
        assert_eq!(super::part1(), 1660158)
    }

    #[test]
    fn part2() {
        assert_eq!(super::part2(), 1604592846)
    }
}
