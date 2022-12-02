use std::fs::File;
use std::io::Read;

#[derive(Eq, Hash, PartialEq)]
enum Hand {
    Rock,
    Paper,
    Scissor,
}

#[derive(Eq, Hash, PartialEq)]
enum Result {
    FirstWin,
    Draw,
    SecondWin,
}

// input type is a list of two hands
type InputPart1 = Vec<(Hand, Hand)>;
type InputPart2 = Vec<(Hand, Result)>;

// output type is an integer
type Output = i32;

// parse the input : String -> Input
// format :
//   two hands separated by a space, for each line
//   hands are represented by A (Rock), B (Paper) or C (Scissor) for the first player
//   and X (Rock), Y (Paper) or Z (Scissor) for the second player
fn parse_input_part1(input: &str) -> InputPart1 {
    let lines = input.lines();
    let mut result: InputPart1 = vec![];

    for line in lines {
        let hands: Vec<&str> = line.split(" ").collect();
        let first_hand = match hands[0] {
            "A" => Hand::Rock,
            "B" => Hand::Paper,
            "C" => Hand::Scissor,
            x => panic!("Invalid first hand : {}", x),
        };
        let second_hand = match hands[1] {
            "X" => Hand::Rock,
            "Y" => Hand::Paper,
            "Z" => Hand::Scissor,
            x => panic!("Invalid second hand : {}", x),
        };
        result.push((first_hand, second_hand));
    }

    result
}

// parse the input : String -> Input
// format :
//   one hand and an expected result separated by a space, for each line
//   hands are represented by A (Rock), B (Paper) or C (Scissor) for the first player
//   and X (Lose), Y (Draw) or Z (Win) for the second player
fn parse_input_part2(input: &str) -> InputPart2 {
    let lines = input.lines();
    let mut result: InputPart2 = vec![];

    for line in lines {
        let w: Vec<&str> = line.split(" ").collect();
        let hand = match w[0] {
            "A" => Hand::Rock,
            "B" => Hand::Paper,
            "C" => Hand::Scissor,
            x => panic!("Invalid hand : {}", x),
        };
        let expected_result = match w[1] {
            "X" => Result::FirstWin,
            "Y" => Result::Draw,
            "Z" => Result::SecondWin,
            x => panic!("Invalid result : {}", x),
        };
        result.push((hand, expected_result));
    }

    result
}

fn simulate(first_hand: &Hand, second_hand: &Hand) -> Result {
    match (first_hand, second_hand) {
        (Hand::Rock, Hand::Rock) => Result::Draw,
        (Hand::Rock, Hand::Paper) => Result::SecondWin,
        (Hand::Rock, Hand::Scissor) => Result::FirstWin,
        (Hand::Paper, Hand::Rock) => Result::FirstWin,
        (Hand::Paper, Hand::Paper) => Result::Draw,
        (Hand::Paper, Hand::Scissor) => Result::SecondWin,
        (Hand::Scissor, Hand::Rock) => Result::SecondWin,
        (Hand::Scissor, Hand::Paper) => Result::FirstWin,
        (Hand::Scissor, Hand::Scissor) => Result::Draw,
    }
}

// shape score :
//  1 for rock
//  2 for paper
//  3 for scissor
fn shape_score(hand: &Hand) -> i32 {
    match hand {
        Hand::Rock => 1,
        Hand::Paper => 2,
        Hand::Scissor => 3,
    }
}

// result score :
//  0 for first player win
//  3 for draw
//  6 for second player win
fn result_score(result: &Result) -> i32 {
    match result {
        Result::FirstWin => 0,
        Result::Draw => 3,
        Result::SecondWin => 6,
    }
}

// solve the problem : Input -> Output
// compute sum of scores for each round
fn solve_part1(input: &InputPart1) -> Output {
    input
        .iter()
        .map(|(first_hand, second_hand)| {
            let result = simulate(first_hand, second_hand);
            shape_score(second_hand) + result_score(&result)
        })
        .sum()
}

// find what to play to achieve expected result
fn find_hand(first_hand: &Hand, result: &Result) -> Hand {
    match (first_hand, result) {
        (Hand::Rock, Result::FirstWin) => Hand::Scissor,
        (Hand::Rock, Result::Draw) => Hand::Rock,
        (Hand::Rock, Result::SecondWin) => Hand::Paper,
        (Hand::Paper, Result::FirstWin) => Hand::Rock,
        (Hand::Paper, Result::Draw) => Hand::Paper,
        (Hand::Paper, Result::SecondWin) => Hand::Scissor,
        (Hand::Scissor, Result::FirstWin) => Hand::Paper,
        (Hand::Scissor, Result::Draw) => Hand::Scissor,
        (Hand::Scissor, Result::SecondWin) => Hand::Rock,
    }
}

// solve the problem : Input -> Output
// find what to play to achieve expected result and then compute sum of scores for each round
fn solve_part2(input: &InputPart2) -> Output {
    input
        .iter()
        .map(|(first_hand, result)| {
            let second_hand = find_hand(first_hand, result);
            let result = simulate(first_hand, &second_hand);
            shape_score(&second_hand) + result_score(&result)
        })
        .sum()
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
