package libs

class IntervalTree {

    class Node(var l : Int, var r : Int, var sum : Int) {
        var lChild : Option[Node] = None
        var rChild : Option[Node] = None
    }

    var root : Option[Node] = None

    def build(l : Int, r : Int) : Unit = {
        this.root = buildHelper(this.root, l, r)
    }

    def update(i : Int, y : Int) : Unit = {
        updateHelper(root, i, y)
    }

    private def updateHelper(rt : Option[Node], i : Int, v : Int) : Unit = {
        rt match {
            case None => None
            case Some(node) => {
                node.sum += v
                if (node.l == i && node.l == node.r) {
                    return
                } else {
                    val mid = (node.l + node.r) >>> 1
                    if (i <= mid) {
                        return updateHelper(node.lChild, i, v)
                    } else {
                        return updateHelper(node.rChild, i, v)
                    }
                }
            }
        }
    }

    def partialSum(i : Int) : Int = {
        partialSumHelper(root, 1, i)
        // partialSumHelper1(root, i)
    }

    private def partialSumHelper1(rt : Option[Node], i : Int) : Int = {
        rt match {
            case None => 0
            case Some(node) => {
                if (node.l >= 0 && node.r <= i) {
                    node.sum
                } else {
                    partialSumHelper1(node.lChild, i) +
                    partialSumHelper1(node.rChild, i)
                }
            }
        }
    }

    private def partialSumHelper(rt : Option[Node], l : Int, r : Int) : Int = {
        rt match {
            case None => 0
            case Some(node) => {
                if (node.l == l && node.r == r) {
                    node.sum
                } else {
                    val mid = (node.l + node.r) >>> 1
                    if (r <= mid) {
                        partialSumHelper(node.lChild, l, r)
                    } else if (l > mid) {
                        partialSumHelper(node.rChild, l, r)
                    } else {
                        partialSumHelper(node.lChild, l, mid) + partialSumHelper(node.rChild, mid + 1, r)
                    }
                }
            }
        }
    }

    private def buildHelper(rt : Option[Node], l : Int, r : Int) : Option[Node] = {
        if (l == r) {
            return Some(new Node(l, r, 0))
        } else {
            var root = rt match {
                case None => new Node(l, r, 0)
                case Some(node) => node
            }
            val mid = (l + r) >>> 1
            root.lChild = buildHelper(root.lChild, l, mid)
            root.rChild = buildHelper(root.rChild, mid + 1, r)
            Some(root)
        }
    }
}