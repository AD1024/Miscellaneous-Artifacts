package libs.Heap
import scala.util.control.Breaks._

class Heap[T <: Comparable[T]] (var pool : List[T], 
            val compare : (T, T) => Boolean) {
    def size(): Int = {
        return this.pool.size;
    }

    def sift_up(index : Int) : Unit = {
        var i = index
        while ((i >> 1) > 0) {
            val fa = i >> 1;
            if (this.compare(this.pool(i), this.pool(fa))) {
                val temp = this.pool(fa)
                this.pool.updated(fa, this.pool(i))
                this.pool.updated(i, temp)
                i = fa

            } else {
                break
            }
        }
    }

    def sift_down(index : Int) : Unit = {
        var i = index
        while ((i << 1) < this.size()) {
            var cur = i
            val lson = (i << 1) + 1
            val rson = (i << 1) + 2
            if (this.compare(this.pool(lson), this.pool(cur)))
                cur = lson
            
            if (rson < this.size() && this.compare(this.pool(rson), this.pool(cur)))
                cur = rson
            
            if (cur != i) {
                val tmp = pool(cur)
                pool.updated(cur, pool(index))
                pool.updated(i, tmp)
                i = cur
            } else {
                break
            }
        }
    }

    def empty() : Boolean = {
        this.size == 0
    }

    def insert(dat : T) = {
        this.pool = this.pool :+ dat
        sift_up(this.size() - 1)
    }

    def pop() : Option[T] = {
        if (this.size() == 0) {
            None
        } else {
            val result = this.pool(0)
            this.pool = this.pool.tail
            pool match {
                case xs :+ last => {
                    pool = last :: xs
                    sift_down(0)
                }
                case Nil => Nil
            }
            Some(result)
        }
    }
}