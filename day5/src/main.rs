use std::fs::File;
use std::io::Read;
use regex::Regex;
use im::vector::Vector;

type Elem = char;
type State = Vector<Vector<Elem>>;

struct Instruction {
    count: usize,
    from: usize,
    to: usize
}

type InputPart1 = (Vec<Instruction>, State);

type InputPart2 = InputPart1;

type Output = String;

// Input string example :
//             [L] [M]         [M]    
//         [D] [R] [Z]         [C] [L]
//         [C] [S] [T] [G]     [V] [M]
// [R]     [L] [Q] [B] [B]     [D] [F]
// [H] [B] [G] [D] [Q] [Z]     [T] [J]
// [M] [J] [H] [M] [P] [S] [V] [L] [N]
// [P] [C] [N] [T] [S] [F] [R] [G] [Q]
// [Z] [P] [S] [F] [F] [T] [N] [P] [W]
//  1   2   3   4   5   6   7   8   9 
// 
// move 7 from 3 to 9
// move 5 from 8 to 9
// move 3 from 9 to 5
// move 6 from 9 to 2
// move 9 from 9 to 3
// move 3 from 7 to 3
// move 8 from 2 to 3
// move 9 from 3 to 1
// move 11 from 3 to 8

fn parse_state(state_part: &str) -> State {
    let state_regex = Regex::new(r"^.(.)...(.)...(.)...(.)...(.)...(.)...(.)...(.)...(.).$").unwrap();
    let zero_state: State = (0..9).map(|_| Vector::new()).collect();

    let parsed_state = 
        state_part
            .lines()
            .map(| l | {
                let captures: Vec<Elem> = state_regex.captures(l).unwrap().iter().skip(1).map(| c | c.unwrap().as_str().chars().next().unwrap()).collect();
                captures
            }).fold(zero_state, | state, captures | {
                captures
                    .iter()
                    .enumerate()
                    .filter(| (_idx, elem) | (*elem).is_alphabetic())
                    .fold(state, | state, (idx, elem) | {
                        let mut new_value = state[idx].clone();
                        let mut new_state = state.clone();
                        new_value.push_back(elem.clone());
                        new_state.set(idx, new_value);
                        new_state
                })
        });


    let actual_state: State = parsed_state.iter().map(| v | v.iter().rev().map(|e| *e).collect::<Vector<Elem>>()).collect();

    actual_state
}

fn parse_instructions(instruction_part: &str) -> Vec<Instruction> {
    instruction_part
        .replace("move ", "")
        .replace(" from ", ",")
        .replace(" to ", ",")
        .lines()
        .map(|l| {
            let a = l.split(",").map(| p | p.parse::<usize>().unwrap()).collect::<Vec<usize>>();

            Instruction { count: a[0], from: a[1] - 1, to: a[2] - 1 }
        })
        .collect()
}

// parse the input : String -> Input
fn parse_input_part1(input: &str) -> InputPart1 {
    let parts: Vec<&str> = input.split("\r\n\r\n").collect();
    let state = parse_state(parts[0]);
    let instruction_part = parse_instructions(parts[1]);

    (instruction_part, state)
}

const PARSE_INPUT_PART2: fn(&str) -> InputPart2 = parse_input_part1;

fn exec_instruction(instruction: &Instruction, state: &State) -> State {
    let mut new_state = state.clone();
    let mut from = new_state[instruction.from as usize].clone();
    let mut to = new_state[instruction.to as usize].clone();

    for _ in 0..instruction.count {
        let elem = from.pop_back().unwrap();
        to.push_back(elem);
    }

    new_state.set(instruction.from as usize, from);
    new_state.set(instruction.to as usize, to);

    new_state
}

fn exec_instructions(instructions: &Vec<Instruction>, state: &State) -> State {
    instructions.iter().fold(state.clone(), | state, instruction | exec_instruction(instruction, &state))
}

fn get_top_line(state: &State) -> String {
    state
    .iter()
    .map(| a | a.last().unwrap())
    .collect::<String>()
}

fn solve_part1(input: &InputPart1) -> Output {
    let result_state = exec_instructions(&input.0, &input.1);

    get_top_line(&result_state)
}

fn exec_instruction_part2(instruction: &Instruction, state: &State) -> State {
    let mut new_state = state.clone();
    let mut from = new_state[instruction.from as usize].clone();
    let mut to = new_state[instruction.to as usize].clone();
    let mut temp = Vector::new();

    for _ in 0..instruction.count {
        let elem = from.pop_back().unwrap();
        temp.push_back(elem);
    }

    for _ in 0..instruction.count {
        let elem = temp.pop_back().unwrap();
        to.push_back(elem);
    }

    new_state.set(instruction.from as usize, from);
    new_state.set(instruction.to as usize, to);

    new_state
}

fn exec_instructions_part2(instructions: &Vec<Instruction>, state: &State) -> State {
    instructions.iter().fold(state.clone(), | state, instruction | exec_instruction_part2(instruction, &state))
}

fn solve_part2(input: &InputPart2) -> Output {
    let result_state = exec_instructions_part2(&input.0, &input.1);

    get_top_line(&result_state)
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
