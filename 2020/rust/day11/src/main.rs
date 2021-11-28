use std::fmt;
use std::fs;

const MAX_Y: usize = 98;
const MAX_X: usize = 98;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Cell {
    Floor,
    EmptySeat,
    OccupiedSeat,
}

impl fmt::Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = match self {
            Cell::Floor => '.',
            Cell::EmptySeat => 'L',
            Cell::OccupiedSeat => '#',
        };
        write!(f, "{}", c)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Grid {
    cell: Vec<Vec<Cell>>,
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in &self.cell {
            for c in line {
                write!(f, "{}", c).unwrap()
            }
            writeln!(f, "").unwrap()
        }
        Ok(())
    }
}

// #[derive(Eq)]
impl Grid {
    fn new() -> Grid {
        Grid {
            cell: vec![vec![Cell::Floor; MAX_X]; MAX_Y],
        }
    }

    // If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    // If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    // Otherwise, the seat's state does not change.
    fn next_state_for_cell(&self, (x, y): (usize, usize)) -> Cell {
        let cnt = self.num_occupied_adjacents((x, y));
        let current = self.cell[y][x];
        let next = match current {
            Cell::EmptySeat if cnt == 0 => Cell::OccupiedSeat,
            Cell::OccupiedSeat if cnt >= 4 => Cell::EmptySeat,
            otherwise => otherwise,
        };
        next
    }

    fn num_occupied_adjacents(&self, (x, y): (usize, usize)) -> i32 {
        let adjs = [
            (-1, -1),
            (-1, 0),
            (-1, 1),
            (0, -1),
            (0, 1),
            (1, -1),
            (1, 0),
            (1, 1),
        ];
        let mut num_occupied = 0;
        for (dy, dx) in adjs {
            let (y2, x2) = (y as i32 + dy, x as i32 + dx);
            if y2 >= 0
                && y2 < MAX_Y as i32
                && x2 >= 0
                && x2 < MAX_X as i32
                && self.cell[y2 as usize][x2 as usize] == Cell::OccupiedSeat
            {
                num_occupied += 1
            }
        }
        num_occupied
    }

    fn next_state(&self) -> Grid {
        let mut next = Grid::new();

        for (y, line) in self.cell.iter().enumerate() {
            for (x, _c) in line.iter().enumerate() {
                next.cell[y][x] = self.next_state_for_cell((x, y));
            }
        }

        next
    }

    fn num_occupied(&self) -> usize {
        let mut cnt = 0;
        for line in self.cell.iter() {
            for c in line.iter() {
                if *c == Cell::OccupiedSeat {
                    cnt += 1;
                }
            }
        }
        cnt
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let mut g = Grid::new();

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            let cell = match c {
                'L' => Cell::EmptySeat,
                '#' => Cell::OccupiedSeat,
                '.' | _ => Cell::Floor,
            };
            g.cell[y][x] = cell;
        }
    }

    let mut history = vec![g];

    let stable = loop {
        let current = history.last().unwrap();
        let next = current.next_state();

        if let Some(_) = history.iter().find(|&s| *s == next) {
            break next;
        } else {
            history.push(next);
        }
    };

    println!("{}", stable.num_occupied());
}
