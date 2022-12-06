use std::fs::File;
use std::io::Read;

type InputPart1 = String;

type InputPart2 = InputPart1;

type Output = usize;

// parse the input : String -> Input
fn parse_input_part1(input: &str) -> InputPart1 {
    input.to_string()
}

const PARSE_INPUT_PART2: fn(&str) -> InputPart2 = parse_input_part1;

fn check_all_different(input: &str) -> bool {
    input.chars().all(|c| input.matches(c).count() == 1)
}

fn find_sequence(input: &str, length: usize) -> Option<usize> {
    input
    .chars()
    .enumerate()
    .map(|(idx, _c)| idx)
    .find(| idx | check_all_different(&input[*idx..*idx + length]))
    .map(|idx| idx + length)
}

fn solve_part1(input: &InputPart1) -> Output {
    find_sequence(input, 4).unwrap_or(0)
}


fn solve_part2(input: &InputPart2) -> Output {
    find_sequence(input, 14).unwrap_or(0)
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
    let input_part2 = PARSE_INPUT_PART2(&raw_input);
    let output_part1 = solve_part1(&input_part1);
    let output_part2 = solve_part2(&input_part2);
    print_output(&output_part1, &output_part2);
}
