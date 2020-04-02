class Node:
    def __init__(self, key, data):
        self.key = key
        self.data = data
    
    def __ge__(self, value):
        return self.data >= value.data
    
    def __le__(self, value):
        return self.key <= value.key
    
    def __gt__(self, value):
        return self.key > value.key

    def __lt__(self, value):
        return self.key < value.key

    def __str__(self):
        return self.__repr__()

    def __add__(self, o):
        return Node(self.key + o.key, self.data + o.data)
    
    def __repr__(self):
        return f'Key: {self.key}, Data: {self.data}'
    


class PriorityQueue:

    def __init__(self, comp=lambda x,y: x < y):
        self.comparator = comp
        self.pool = [None]   # None guard
        self.ptr  = 1
        self.lson = lambda i: i << 1
        self.rson = lambda i: i << 1 | 1
        self.fa   = lambda i: i >> 1

    def _push_up(self, i):
        while self.fa(i) >= 1:
            fa = self.fa(i)
            if self.comparator(self.pool[fa], self.pool[i]):
                break
            else:
                self.pool[fa], self.pool[i] = self.pool[i], self.pool[fa]
                i = fa
    

    def _push_down(self):
        i = 1
        ptr = self.ptr
        while self.lson(i) < ptr:
            rson = self.rson(i)
            lson = self.lson(i)
            cur  = i
            if self.comparator(self.pool[lson], self.pool[i]):
                cur = lson                
            
            if rson < ptr and self.comparator(self.pool[rson], self.pool[i]):
                cur = rson
            
            if i != cur:
                self.pool[cur], self.pool[i] = self.pool[i], self.pool[cur]
                i = cur
            else:
                break
    
    def insert(self, node):
        self.pool.append(node)
        self._push_up(self.ptr)
        self.ptr += 1
    
    def pop(self):
        assert not self.empty()
        result = self.pool[1]
        self.pool[1] = self.pool[self.ptr - 1]
        self.ptr -= 1
        self._push_down()
        self.pool.pop()
        return result

    def top(self):
        assert not self.empty()
        return self.pool[1]
    
    def empty(self):
        return len(self.pool) == 1
    
    def size(self):
        return len(self.pool) - 1

minH = PriorityQueue(lambda x,y: x < y)
maxH = PriorityQueue(lambda x,y: x > y)

N = int(input())
for i in range(N):
    d = int(input())
    if minH.size() < (N + 1) // 2:
        minH.insert(d)
    elif d > minH.top():
        minH.pool[1] = d
        minH._push_down()
    
    if maxH.size() < (N + 1) // 2:
        maxH.insert(d)
    elif d < maxH.top():
        maxH.pool[1] = d
        maxH._push_down()

for i in range((N + 1) // 2):
    print(f'min {minH.top()} max {maxH.top()}')
    if minH.top() == maxH.top():
        print(f'Med: {minH.top()}')
        minH.pop()
        maxH.pop()
    elif (minH.size() + maxH.size()) % 2 == 0:
        print(f'Med: {(minH.top() + maxH.top()) / 2}')
        maxH.pop()
        minH.pop()
    elif minH.top() < maxH.top():
        print(f'Med: {minH.top()}')
        minH.pop()
    else:
        print(f'Med: {maxH.top()}')
        maxH.pop()
