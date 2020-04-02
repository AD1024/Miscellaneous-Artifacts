import libs.Heap._
import libs.IntervalTree

object Main {
    def main(args: Array[String]): Unit = {
        println("He")
        // val testHeap = new Heap[Integer](List(), (x : Integer, y : Integer) => x < y)
        // for (i <- 10 to 1 by -1) {
        //     testHeap.insert(i)
        // }

        // while (!testHeap.empty()) {
        //     println(testHeap.pop())
        // }
        val interval = new IntervalTree()
        interval.build(1, 10)
        for (i <- 1 to 10) {
            interval.update(i, i)
        }
        println(interval.partialSum(10))
    }
}