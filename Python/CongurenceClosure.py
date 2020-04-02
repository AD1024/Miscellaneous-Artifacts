from functools import reduce

class Node:
    def __init__(self, fn,*args, **kwargs):
        super().__init__(*args, **kwargs)
        self.fn = fn
        self.ccpar = set()
        self.find = self
        self.children = []
    def __repr__(self):
        return f'fn: {self.fn}; childern: {self.children}'

def clause_to_node(clause, graph:dict) -> Node:
    if (clause in graph.keys()):
        return graph[clause]
    if type(clause) == tuple:
        func, *args = clause
        node = Node(func)
        node.children = list(map(lambda x: clause_to_node(x, graph), args))
        for i in range(0, len(node.children)):
            node.children[i].ccpar.add(node)
        graph[clause] = node
        return node
    else:
        graph[clause] = Node(clause)
        return graph[clause]

def find(node:Node) -> Node:
    while node.find != node:
        node = node.find
    return node

def same_closure(f1:Node, f2:Node) -> bool:
    return find(f1) == find(f2)

def union(f1, f2):
    r1 = find(f1)
    r2 = find(f2)
    r1.find = r2
    r2.ccpar = r2.ccpar.union(r1.ccpar)
    r1.ccpar = set()

def congurence(f1:Node, f2:Node) -> Node:
    return f1.fn == f2.fn \
            and len(f1.children) == len(f2.children) \
            and reduce(lambda x, y: x and y, map(lambda x: same_closure(*x), zip(f1.children, f2.children)))

def cartisian(s1:set, s2:set):
    return set(((x, y) for x in s1 for y in s2))

def merge(f1:Node, f2:Node) -> bool:
    r1 = find(f1)
    r2 = find(f2)
    if r1 != r2:
        par1 = r1.ccpar.copy()
        par2 = r2.ccpar.copy()
        union(r1, r2)
        for (p1, p2) in cartisian(par1, par2):
            if not same_closure(p1, p2) and congurence(p1, p2):
                merge(p1, p2)

def formulae_to_map(formulae):
    graph = dict()
    for each in formulae:
        _, lhs, rhs = each
        clause_to_node(lhs, graph)
        clause_to_node(rhs, graph)
    return graph

def solve(formulae):
    graph = formulae_to_map(formulae)
    for each in formulae:
        assert len(each) == 3
        rel, lhs, rhs = each
        if rel == '=':
            merge(graph.get(lhs), graph.get(rhs))
    
    print('UNSAT' if reduce(lambda x,y: x and y, map(lambda x: same_closure(graph[x[1]], graph[x[2]]), 
                              filter(lambda x: x[0] == '!=', formulae)), True) else 'SAT')
    return graph

# Simple
F0 = [('=', 'a', 'b'), ('!=', 'a', 'c'), ('=', 'b', 'c')]

# UNSAT
F1 = [('=', ('f', ('f' ,('f', 'a'))), 'a'), ('=', ('f', ('f', ('f', ('f', ('f', 'a'))))),'a'), ('!=', ('f', 'a'), 'a')]

# SAT
F2 = [('=',  ('f', ('g', 'x')), ('g', ('f', 'x'))), 
      ('=',  ('f', ('g', ('f', 'y'))), 'x'), 
      ('=',  ('f', 'y'), 'x'), 
      ('!=', ('g', 'x'), 'x')]

F3 = [('=', ('f', ('g', 'x')), ('g', ('f', 'x'))),
      ('=', ('f', ('g', ('f', 'y'))), 'x'),
      ('=', ('f', 'y'), 'x'),
      ('!=',('g', ('f', 'x')), 'x')]

F4 = [  ('=', ('f', 'a'), 'b'), 
        ('=', ('f', 'b'), 'c'),
        ('!=', ('f', ('f', ('f', 'a'))), 'a'), 
        ('=', ('f', 'c'), 'a')]