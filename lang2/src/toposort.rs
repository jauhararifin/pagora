use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

pub fn toposort<'a, T: Eq + Hash>(adjlist: Vec<(&'a T, Vec<&'a T>)>) -> ToposortResult<'a, T> {
    let nodes = adjlist.iter().map(|(node, _)| *node);
    let adjlist: Vec<(&'a T, &Vec<&'a T>)> =
        adjlist.iter().map(|(item, deps)| (*item, deps)).collect();
    let mut instance = Toposort::new(&adjlist);
    for node in nodes {
        if !instance.visited.contains(node) {
            instance.dfs(node);
        }
    }
    ToposortResult {
        orders: instance.orders,
        cycles: instance.cycles,
    }
}

struct Toposort<'a, 'b, T> {
    adjlist: HashMap<&'a T, &'b Vec<&'a T>>,
    visited: HashSet<&'a T>,
    has_cycle: HashSet<&'a T>,
    in_cycle: HashSet<&'a T>,
    next_map: HashMap<&'a T, &'a T>,
    chain: Vec<&'a T>,
    orders: Vec<&'a T>,
    cycles: Vec<Vec<&'a T>>,
}

pub struct ToposortResult<'a, T> {
    pub orders: Vec<&'a T>,
    pub cycles: Vec<Vec<&'a T>>,
}

impl<'a, 'b, T: Eq + Hash> Toposort<'a, 'b, T> {
    fn new<'c>(adjlist: &'c [(&'a T, &'b Vec<&'a T>)]) -> Self {
        let adjlist = HashMap::from_iter(adjlist.iter().map(|(a, b)| (*a, *b)));
        let visited = HashSet::new();
        let has_cycle = HashSet::new();
        let in_cycle = HashSet::new();
        let next_map = HashMap::new();
        let chain = vec![];
        let order = vec![];
        let cycle = vec![];
        Self {
            adjlist,
            visited,
            has_cycle,
            in_cycle,
            next_map,
            chain,
            orders: order,
            cycles: cycle,
        }
    }

    fn dfs(&mut self, node: &'a T) {
        self.visited.insert(node);
        self.chain.push(node);
        let neighbors = *self.adjlist.get(node).unwrap();
        for item in neighbors {
            let item = *item;
            self.next_map.insert(node, item);
            if self.has_cycle.contains(item) {
                self.has_cycle.insert(node);
            } else if self.next_map.contains_key(item) {
                self.populate_cycle(item);
            } else if !self.visited.contains(item) {
                self.dfs(item);
                if self.in_cycle.contains(item) || self.has_cycle.contains(item) {
                    self.has_cycle.insert(node);
                }
            }
        }
        self.next_map.remove(node);
        if !self.in_cycle.contains(node) && !self.has_cycle.contains(node) {
            self.orders.push(node);
        }
    }

    fn populate_cycle(&mut self, node: &'a T) {
        let mut cycle = vec![node];
        self.in_cycle.insert(node);
        self.has_cycle.insert(node);

        let mut item = *self.next_map.get(node).unwrap();
        loop {
            self.in_cycle.insert(item);
            self.has_cycle.insert(item);
            if item == node {
                break;
            }
            cycle.push(item);
            item = *self.next_map.get(item).unwrap();
        }
        self.cycles.push(cycle);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() {
        let node1 = (&1, vec![&2, &3]);
        let node2 = (&2, vec![&4, &5]);
        let node3 = (&3, vec![&5, &6]);
        let node4 = (&4, vec![&7, &8]);
        let node5 = (&5, vec![]);
        let node6 = (&6, vec![&8]);
        let node7 = (&7, vec![&9, &10]);
        let node8 = (&8, vec![&2]);
        let node9 = (&9, vec![]);
        let node10 = (&10, vec![&2]);
        let node11 = (&11, vec![&10]);
        let node12 = (&12, vec![&5]);
        let node13 = (&13, vec![&14]);
        let node14 = (&14, vec![]);
        let adjlist = vec![
            node1, node2, node3, node4, node5, node6, node7, node8, node9, node10, node11, node12,
            node13, node14,
        ];
        let result = toposort(adjlist);
        assert_eq!(result.orders, vec![&9, &5, &12, &14, &13]);
        assert_eq!(result.cycles, vec![vec![&2, &4, &7, &10],]);
    }
}
