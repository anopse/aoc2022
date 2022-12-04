use std::fs::File;
use std::io::Read;
use std::ops::RangeInclusive;

type InputPart1 = Vec<(RangeInclusive<i32>,RangeInclusive<i32>)>;

type InputPart2 = InputPart1;

// output type is an integer
type Output = usize;

// parse the input : String -> Input
// format : 1-3,5-8
fn parse_input_part1(input: &str) -> InputPart1 {
    input.lines().map(| l | {
        let values: Vec<i32> = l.replace(",", "-").split('-').map(| s | s.parse::<i32>().unwrap()).collect();
        
        (values[0]..=values[1], values[2]..=values[3])
    }).collect()
}

const parse_input_part2: fn(&str) -> InputPart2 = parse_input_part1;

fn is_range_included_inside(a: &RangeInclusive<i32>, b: &RangeInclusive<i32>) -> bool {
    a.contains(&b.start()) && a.contains(&b.end()) || b.contains(&a.start()) && b.contains(&a.end())
}

fn solve_part1(input: &InputPart1) -> Output {
    input.iter().filter(| (a, b) | is_range_included_inside(a, b)).count()
}

fn is_range_overlapping(a: &RangeInclusive<i32>, b: &RangeInclusive<i32>) -> bool {
    a.contains(&b.start()) || a.contains(&(b.end())) || b.contains(&a.start()) || b.contains(&(a.end()))
}

fn solve_part2(input: &InputPart2) -> Output {
    input.iter().filter(| (a, b) | is_range_overlapping(a, b)).count()
}

// print the output : Output -> ()
fn print_output(output_part1: &Output, output_part2: &Output) {
    println!("part1: {}", output_part1);
    println!("part2: {}", output_part2);
}

// read input.txt file
fn read_input() -> String {
    let mut file = File::open("input.txt").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents
}

fn main() {
    let raw_input = read_input();
    let input_part1 = parse_input_part1(&raw_input);
    let input_part2 = parse_input_part2(&raw_input);
    let output_part1 = solve_part1(&input_part1);
    let output_part2 = solve_part2(&input_part2);
    print_output(&output_part1, &output_part2);
}
