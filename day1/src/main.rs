use std::fs::File;
use std::io::Read;

// input type is a list of list of integers
type Input = Vec<Vec<i32>>;

// output type is an integer
type Output = i32;

// parse the input : String -> Input
// format : one list per integer, separated by a blank newline for each group
fn parse_input(input: &str) -> Input {
    let lines = input.lines();
    // split lines into group by blank lines
    let mut groups: Input = Vec::new();
    let mut group = Vec::new();
    for line in lines {
        if line.is_empty() {
            groups.push(group);
            group = Vec::new();
        } else {
            // parse line as an integer
            let number = line.parse::<i32>().unwrap();
            group.push(number);
        }
    }

    groups.push(group);
    groups
}

// solve the problem : Input -> Output
// find the maximum group sum value
fn solve_part1(input: &Input) -> Output {
    input
        .iter()
        .map(|group| {
            group
                .iter()
                .fold(0, |acc, x| acc + x)
        })
        .fold(0, |acc, x| acc.max(x))
}

// solve the problem : Input -> Output
// find the top 3 maximum group sum value and sum them
fn solve_part2(input: &Input) -> Output {
    let mut groups_sum: Vec<i32> = input
        .iter()
        .map(|group| {
            group
                .iter()
                .fold(0, |acc, x| acc + x)
        })
        .collect();

    // sort groupSums desc
    groups_sum.sort_by(|a, b| b.cmp(a));

    // take the first 3 elements
    groups_sum.iter().take(3).sum()
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
    let input = parse_input(&raw_input);
    let output_part1 = solve_part1(&input);
    let output_part2 = solve_part2(&input);
    print_output(&output_part1, &output_part2);
}
