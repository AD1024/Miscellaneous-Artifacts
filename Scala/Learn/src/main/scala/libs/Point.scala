package libs

class Point(var x : Double, var y : Double) {
    def distanceSquareTo(p : Point) : Double = {
        Math.pow(x - p.x, 2) + Math.pow(y - p.y, 2)
    }
}