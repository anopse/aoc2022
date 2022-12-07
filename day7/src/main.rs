use std::fs::File;
use std::io::Read;
use im::Vector;
use regex::Regex;
use lazy_static::lazy_static;

#[derive(Debug, Clone)]
enum LsResultEntry {
    File(String, u32),
    Dir(String),
}

#[derive(Debug, Clone)]
enum CdCommand {
    Root,
    Parent,
    Child(String),
}

#[derive(Debug, Clone)]
enum Command {
    Ls(Vector<LsResultEntry>),
    Cd(CdCommand),
}

#[derive(Debug, Clone)]
enum ParseResult<'life> {
    Success(Command, &'life str),
    Failure,
}

type InputPart1 = Vector<Command>;

type InputPart2 = InputPart1;

type Output = u32;

fn parse_cd_line<'life>(input: &'life str) -> ParseResult<'life> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?s)\A\$ cd ([a-zA-Z0-9_/\.]+)\r?\n?").unwrap();
    }

    RE.captures(input)
        .and_then(|captures| {
            let whole = captures.get(0).map(|m| m.as_str())?;
            let arg = captures.get(1).map(|m| m.as_str())?;
            let rest = input.split_at(whole.len()).1;

            Some((rest, arg))
        })
        .map(|(rest, arg)| {
            let cmd = match arg {
                ".." => Command::Cd(CdCommand::Parent),
                "/" => Command::Cd(CdCommand::Root),
                x => Command::Cd(CdCommand::Child(x.to_string())),
            };

            ParseResult::Success(cmd, rest)
        })
        .unwrap_or(ParseResult::Failure::<'life>)
}

fn parse_ls_entries(input: &str) -> Option<Vector<LsResultEntry>> {
    input.lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            let words = l.split(" ").collect::<Vec<&str>>();

            match words.as_slice() {
                ["dir", dir_name] => Some(LsResultEntry::Dir(dir_name.to_string())),
                [rsize, name] => rsize.parse::<u32>()
                            .map(|size| LsResultEntry::File(name.to_string(), size))
                            .ok(),
                _ => None
            }
        })
        .collect::<Vector<Option<LsResultEntry>>>()
        .into_iter()
        .collect::<Option<Vector<LsResultEntry>>>()
}

fn parse_ls_line<'life>(input: &'life str) -> ParseResult<'life> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"(?s)\A\$ ls\r\n(.*?)\r?\n?(\$|\z)").unwrap();
    }

    RE.captures(input)
        .and_then(|captures| {
            let whole = captures.get(0).map(|m| m.as_str())?;
            let arg = captures.get(1).map(|m| m.as_str())?;
            let eof = whole.len() == input.len();
            let rest = input.split_at(whole.len() - if eof { 0 } else { 1 }).1;

            Some((rest, arg))
        })
        .and_then(|(rest, arg)| {
            let entries = parse_ls_entries(arg);
            let cmd = Command::Ls(entries.unwrap());

            Some(ParseResult::Success(cmd, rest))
        })
        .unwrap_or(ParseResult::Failure)
}

fn parse_commands(input: &str) -> Vector<Command> {
    let mut commands = Vector::new();
    let mut rest = input;

    while (!rest.is_empty()) && (rest != "\r\n") {
        match parse_cd_line(rest) {
            ParseResult::Success(cmd, new_rest) => {
                commands.push_back(cmd);
                rest = new_rest;
            },
            ParseResult::Failure =>
                match parse_ls_line(rest) {
                    ParseResult::Success(cmd, new_rest) => {
                        commands.push_back(cmd);
                        rest = new_rest;
                    },
                    ParseResult::Failure => {
                        panic!("Failed to parse input :\n{}", rest);
                    }
                }
        }
    }

    commands
}

// parse the input : String -> Input
fn parse_input_part1(input: &str) -> InputPart1 {
    parse_commands(input)
}

const PARSE_INPUT_PART2: fn(&str) -> InputPart2 = parse_input_part1;


#[derive(Debug, Clone)]
enum DirEntry {
    File(String, u32),
    Dir(String, Vector<DirEntry>),
}

#[derive(Debug, Clone)]
struct State {
    cwd: Vector<String>,
    root: Vector<DirEntry>,
}

fn interpret_cd_command(state: State, cmd: &CdCommand) -> State {
    match cmd {
        CdCommand::Root => State {
            cwd: Vector::new(),
            root: state.root,
        },
        CdCommand::Parent => {
            let mut new_cwd = state.cwd;
            new_cwd.pop_back();

            State {
                cwd: new_cwd,
                root: state.root,
            }
        },
        CdCommand::Child(child) => {
            let mut new_cwd = state.cwd;
            new_cwd.push_back(child.clone());

            State {
                cwd: new_cwd,
                root: state.root,
            }
        }
    }
}

fn ls_entry_to_dir_entry(entry: LsResultEntry) -> DirEntry {
    match entry {
        LsResultEntry::File(name, size) => DirEntry::File(name, size),
        LsResultEntry::Dir(name) => DirEntry::Dir(name, Vector::new()),
    }
}

fn map_cwd_recur(path: Vector<String>, f: &impl Fn(Vector<DirEntry>) -> Vector<DirEntry>, entries: Vector<DirEntry>) -> Vector<DirEntry> {
    match path.is_empty() {
        true => f(entries),
        false => {
            let target = path.front().unwrap();

            let entries_ref = &entries;

            let entry = entries_ref.into_iter().find(|e| match e {
                DirEntry::Dir(name, _) => name == target,
                _ => false
            });

            let entry_or_empty = (entry.map(|e| e.clone())).unwrap_or(DirEntry::Dir(target.clone(), Vector::new()));

            match entry_or_empty {
                DirEntry::Dir(name, sub_entries) => {
                    let mut rec_path = path.clone();
                    rec_path.pop_front();
                    let new_entries = map_cwd_recur(rec_path, f, sub_entries);

                    entries_ref.into_iter().map(|e| match e {
                        DirEntry::Dir(sub_name, _) if *sub_name == name => DirEntry::Dir(sub_name.clone(), new_entries.clone()),
                        _ => e.clone()
                    }).collect()
                },
                _ => panic!("Tried to recurse into a non-directory")
            }
        }
    }
}

fn map_cwd(state: State, f: &impl Fn(Vector<DirEntry>) -> Vector<DirEntry>) -> State {
    let new_root = map_cwd_recur(state.cwd.clone(), f, state.root);

    State {
        cwd: state.cwd,
        root: new_root,
    }
}

fn interpret_ls_command(state: State, result_entries: Vector<LsResultEntry>) -> State {
    let ls_entries = result_entries.into_iter().map(ls_entry_to_dir_entry).collect::<Vector<DirEntry>>();

    map_cwd(state, &|entries| {
        let mut new_entries = entries.clone();

        ls_entries.clone().into_iter().for_each(|new_entry| {
            match new_entry {
                DirEntry::File(new_file_name, new_file_size) => {
                    let existing_entry = new_entries.clone().into_iter().find(|e| match e {
                        DirEntry::File(existing_name, _) => *existing_name == new_file_name,
                        _ => false
                    });

                    match existing_entry {
                        Some(_) => (),
                        None => new_entries.push_back(DirEntry::File(new_file_name, new_file_size))
                    }
                },
                DirEntry::Dir(new_dir_name, new_dir_entries) => {
                    let existing_entry = new_entries.clone().into_iter().find(|e| match e {
                        DirEntry::Dir(existing_name, _) => *existing_name == new_dir_name,
                        _ => false
                    });

                    match existing_entry {
                        Some(_) => (),
                        None => new_entries.push_back(DirEntry::Dir(new_dir_name, new_dir_entries))
                    }
                }
            }
        });

        new_entries
    })
}

fn build_file_tree(commands: &Vector<Command>) -> Vector<DirEntry> {
    let init = State {
        cwd: Vector::new(),
        root: Vector::new(),
    };

    let resulting_state = commands.into_iter().fold(init, |state, cmd| {
        match cmd {
            Command::Cd(cd_command) => interpret_cd_command(state, cd_command),
            Command::Ls(ls_result) => interpret_ls_command(state, ls_result.clone()),
        }
    });

    resulting_state.root
}

fn fold_dir_entry<T>(entry: &DirEntry, acc: T, f: &impl Fn(&DirEntry, T) -> T) -> T {
    match entry {
        DirEntry::File(_, _) => f(entry, acc),
        DirEntry::Dir(_, entries) => {
            let new_acc = entries.into_iter().fold(acc, |acc, entry| fold_dir_entry(entry, acc, f));
            f(entry, new_acc)
        }
    }
}

fn entry_size(entry: &DirEntry) -> u32 {
    fold_dir_entry(entry, 0, &|entry, acc| match entry {
        DirEntry::File(_, size) => acc + *size,
        _ => acc
    })
}

fn solve_part1(input: &InputPart1) -> Output {
    let file_tree = build_file_tree(input);
    
    file_tree
        .into_iter()
        .map(|entry| {
            fold_dir_entry(&entry, 0, &|child, acc| {
                match child {
                    DirEntry::Dir(_name, _) => {
                        let size = entry_size(&child);
                        //println!("{}: {} ", _name, size);
                        acc + if size <= 100000 { size } else { 0 }
                    },
                    DirEntry::File(_, _) => acc
                }
            })
        }).sum()
}

fn solve_part2(input: &InputPart2) -> Output {
    let file_tree = build_file_tree(input);

    const TOTAL_SIZE: u32 = 70000000;
    const REQUIRED_SPACE: u32 = 30000000;
    let used_space: u32 = file_tree
        .clone()
        .into_iter()
        .map(|entry| entry_size(&entry))
        .sum();

    let free_space = TOTAL_SIZE - used_space;
    let space_to_free = REQUIRED_SPACE - free_space;
    println!("Free space: {}", free_space);
    println!("Space to free: {}", space_to_free);
    
    let all_sizes: Vec<u32> = file_tree
        .into_iter()
        .map(|entry| {
            fold_dir_entry(&entry, Vector::new(), &|child, acc| {
                match child {
                    DirEntry::Dir(_name, _) => {
                        let size = entry_size(&child);
                        let mut new_results = acc.clone();
                        new_results.push_back(size);
                        new_results
                    },
                    DirEntry::File(_, _) => acc
                }
            })
        })
        .flatten()
        .collect();

    let eligible_sizes: Vector<u32> = all_sizes.into_iter().filter(|size| *size >= space_to_free).collect();

    let min_dir_size = eligible_sizes.into_iter().min().unwrap();

    min_dir_size
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
    //test();
    let raw_input = read_input();
    let input_part1 = parse_input_part1(&raw_input);
    let input_part2 = PARSE_INPUT_PART2(&raw_input);
    let output_part1 = solve_part1(&input_part1);
    let output_part2 = solve_part2(&input_part2);
    print_output(&output_part1, &output_part2);
}
