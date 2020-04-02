package libs

class UnionFind(var pool : List[Int]) {
    private def find(x : Int) : Int = {
        if (pool(x) == x) x
        else find(pool(x))
    }

    def connect(p : Int, q : Int) {
        val fp = find(p)
        val fq = find(q)
        pool = pool.updated(fp, fq)
    }

    def isconnectd(p : Int, q : Int) : Bool = {
        find(p) == find(q)
    }
}