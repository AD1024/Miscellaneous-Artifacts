mod libs;
use libs::heap;
use std::iter;

fn main() {
    let mut min_heap : heap::Heap<i32> = heap::Heap::new();
    min_heap.insert(1);
    min_heap.insert(3);
    min_heap.insert(5);
    min_heap.insert(4);
    while !min_heap.empty() {
        match min_heap.pop() {
            Some(data) => {
                println!("{}", data)
            }
            None => continue
        }
    }
    for i in (1..100).rev() {
        min_heap.insert(i)
    }
    while !min_heap.empty() {
        match min_heap.pop() {
            Some(data) => {
                if data % 2 == 0 {
                    println!("Oof: {}", data)
                }
            }
            None => continue
        }
    }
    println!("Hello, world!");
}
