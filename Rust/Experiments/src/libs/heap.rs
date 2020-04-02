use std;
use std::mem;
use std::clone;

#[derive(Debug)]
pub struct Heap<T> {
    size: usize,
    pool: Vec<T>
}

impl<T : Ord + Clone> Heap<T> {
    pub fn get_size(&self) -> usize {
        self.pool.len()
    }

    fn sift_up(&mut self, mut index: usize) {
        while (index >> 1) >= 0 {
            if self.pool[index] < self.pool[index >> 1] {
                self.pool.swap(index, index >> 1);
                index >>= 1;
            } else {
                break;
            }
        }
    }

    fn sift_down(&mut self, mut index: usize) {
        while ((index << 1) + 1) < self.get_size() {
            let lson = (index << 1) + 1;
            let rson = (index << 1) + 2;
            let mut cur = index;
            if self.pool[cur] > self.pool[lson] {
                cur = lson;
            }
            if rson < self.get_size()
                && self.pool[cur] > self.pool[rson] {
                cur = rson;
            }
            if cur != index {
                self.pool.swap(cur, index);
                index = cur;
            } else {
                break;
            }
        }
    }

    pub fn insert(&mut self, datum: T) {
        self.size += 1;
        self.pool.push(datum);
        self.sift_up(self.size - 1);
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.get_size() > 0 {
            let ret = Some(self.pool[0].clone());
            match self.pool.pop() {
                Some(datum) => {
                    self.size -= 1;
                    if self.get_size() > 0 {
                        self.pool[0] = datum;
                        self.sift_down(0);
                    }
                    ret
                }
                None => None
            }
        } else {
            None
        }
    }

    pub fn empty(&self) -> bool {
        self.pool.len() == 0
    }

    pub fn new() -> Heap<T> {
        let ret = Heap {size : 0, pool : std::vec::Vec::new()};
        ret
    }
}