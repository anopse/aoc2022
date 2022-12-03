use std::fs::File;
use std::io::Read;

// input type is a list two strings
type InputPart1 = Vec<(String, String)>;

// input type is a list three strings
type InputPart2 = Vec<(String, String, String)>;

// output type is an integer
type Output = i32;

// parse the input : String -> Input
// format : multiple lines that needs to be split in the middle (no separator)
fn parse_input_part1(input: &str) -> InputPart1 {
    input.lines().map(| l | {
            let middle = l.len() / 2;
            (l[..middle].to_string(), l[middle..].to_string())
        }
    ).collect()
}

// parse the input : String -> Input
// format : multiple lines grouped by 3 lines
fn parse_input_part2(input: &str) -> InputPart2 {
    let lines: Vec<&str> = input.lines().collect();
    
    lines.chunks(3).map(| l | {
            (l[0].to_string(), l[1].to_string(), l[2].to_string())
        }
    ).collect()
}

// map a to z => 1 to 26 and A to Z => 27 to 52
fn map_char(c: char) -> i32 {
    match c {
        'a'..='z' => c as i32 - 'a' as i32 + 1,
        'A'..='Z' => c as i32 - 'A' as i32 + 27,
        _ => panic!("Invalid character {}", c)
    }
}

fn find_repeated_char(s1: &str, s2: &str) -> Option<char> {
    s1.find(| c | s2.contains(c)).and_then(| i | s1.chars().nth(i))
}

// solve the problem : Input -> Output
//   foreach pair of strings, find the first character that is repeated in both strings and map it to a number then sum all numbers
fn solve_part1(_input: &InputPart1) -> Output {
    _input.iter().map(| (s1, s2) | {
        find_repeated_char(s1, s2).map(map_char).unwrap_or(0)
    }).sum()
}

fn find_repeated_char3(s1: &str, s2: &str, s3: &str) -> Option<char> {
    s1.find(| c | s2.contains(c) && s3.contains(c)).and_then(| i | s1.chars().nth(i))
}

// solve the problem : Input -> Output
//  foreach triplet of strings, find the first character that is repeated in all strings and map it to a number then sum all numbers
fn solve_part2(input: &InputPart2) -> Output {
    input.iter().map(| (s1, s2, s3) | {
        find_repeated_char3(s1, s2, s3).map(map_char).unwrap_or(0)
    }).sum()
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
