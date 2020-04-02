use std::vec::Vec;
use std::Hash;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug + Eq)]
struct UnionFindNode<T> {
    data : T,
    father : Option<UnionFindNode<T>>
}

#[derive(Debug)]
struct UnionFind<T> {
    root : Option<UnionFindNode<T>>,
    item_map : HashMap<T, &UnionFindNode<T>>
}

impl<T: Hash> UnionFind<T> {
    pub fn new(num_sets : usize) -> UnionFind<T> {
        UnionFind {root : None, item_map : HashMap::new()}
    }

    pub fn same_set(&self, fi : T, se : T) -> bool {
        let fi_idx = self.item_idx.get(&fi);
        let se_idx = self.item_idx.get(&se);
        match (fi_idx, se_idx) {
            (Some(node_a), Some(node_b)) => {
                self.find_root(node_a) == self.find_root(node_b)
            }
            (_, _) => false
        }
    }

    fn find_root(&self, x : &UnionFindNode<T>) -> &UnionFindNode<T> {
        match &x.father {
            None => x,
            Some(node) => find_root(self.find_root(node))
        }
    }
}