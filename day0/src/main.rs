fn main() {
    // create a list from 0 to 10
    let list: Vec<i32> = (0..10).collect::<Vec<i32>>();

    // map list to square
    let square_list: Vec<i32> = list.iter().map(|x| x * x).collect();

    // print the list
    println!("{:?}", square_list);
}
