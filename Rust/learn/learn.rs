use std::io;

fn f(string : &String) {
    println!("{}", string);
}

fn g(string : &str) {
    println!("Good : {}", string);
}

fn use_slice(slice : &str) -> &str {
    for (i, &j) in slice.as_bytes().iter().enumerate() {
        println!("{} {}", i, j);
    }
    slice
}

fn times(x : i32, y : i32) -> i32 {
    x * y
}

#[derive(Debug)]
struct Rect {
    height : i32,
    width : i32
}

impl Rect {
    fn can_fit(&self, rect : &Rect) -> bool {
        self.height >= rect.height && self.width >= rect.width
    }

    fn new(h : i32, w : i32) -> Rect {
        Rect {height: h, width: w}
    }
}

#[derive(Debug)]
enum Either<L, R> {
    Left(L),
    Right(R)
}

fn modify(num : &u32) {
    println!("{}", num);
}

fn main() {
    let left : Either<i32, Rect> = Either::Left(1);
    let data = Some(1);
    if let Some(x) = data {
        println!("{}", x);
    }
    println!("{:?}", left);
    println!("aaaaaa");
    let mut arr = [1, 2, 3, 4, 5];
    let s1 : &[usize] = &arr;
    for each in s1.iter() {
        println!("{}", each);
    }
    let s2 = &mut arr;
    let mut msg = String::from("Ahhh");
    f(&msg);
    take_charge(msg);
    println!("Second: {}", msg);
    use_slice(&mut msg);
    // g(&msg[..]);
    println!("{}", msg);
    // let mut x = String::new();
    // let mut y = String::new();
    // io::stdin().read_line(&mut x).expect("Failed");
    // io::stdin().read_line(&mut y).expect("Failed");
    // let x : i32 = x.trim().parse().expect("Oof");
    // let y : i32 = y.trim().parse().expect("Oof");
    // println!("Sum : {}", x + y);
    println!("New Rect: {:?}", Rect::new(2, 3));
    println!("Can fit: {}", Rect::new(2, 3).can_fit(&Rect::new(1, 1)));

    let mut vec = vec![1, 2, 3, 4, 5];
    let mut elem = &vec[2];
    for each in vec.iter() {
        println!("{}", each);
    }
    let k : &u32 = &vec[2];
    println!("Vector K : {}", k);

    modify(elem);

    let mut op = String::new();
    // io::stdin()::read_line(&mut op).expect("Failed");
    let mut op : i32 = op.trim().parse().expect("Failed to parse");
    // while op > 0 {
    //     let cmd = String::new();
    //     io::stdin().read_line(&mut cmd).expect("");
    //     let 
    // }
}