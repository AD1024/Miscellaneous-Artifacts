(push)
    (declare-const x1y1 Int)
    (declare-const x1y2 Int)
    (declare-const x2y1 Int)
    (declare-const x2y2 Int)
    (assert
        (and
            (not ( = x1y1 x1y2))
            (not ( = x1y1 x2y1))
            (not ( = x1y2 x2y2))
            (not ( = x2y1 x2y2))
            (>= x1y1 0)
            (<= x1y1 9)
            (>= x1y2 0)
            (<= x1y2 9)
            (>= x2y1 0)
            (<= x2y1 9)
            (>= x2y2 0)
            (<= x2y2 9)))
    (check-sat)
    (get-model)
(pop)

;; 2
;; 1 0 0 4
;; 3 0 1 0
;; 0 1 0 3
;; 0 0 2 0

(push)
    (declare-const x0y0 Int)
    (declare-const x0y1 Int)
    (declare-const x0y2 Int)
    (declare-const x0y3 Int)

    (declare-const x1y0 Int)
    (declare-const x1y1 Int)
    (declare-const x1y2 Int)
    (declare-const x1y3 Int)

    (declare-const x2y0 Int)
    (declare-const x2y1 Int)
    (declare-const x2y2 Int)
    (declare-const x2y3 Int)

    (declare-const x3y0 Int)
    (declare-const x3y1 Int)
    (declare-const x3y2 Int)
    (declare-const x3y3 Int)

    (assert
        (and
            (not (= x0y0 x0y1))
            (not (= x0y0 x1y0))
            (not (= x0y0 x1y1))
            (not (= x0y1 x1y0))
            (not (= x0y1 x1y1))
            (not (= x1y0 x1y1))))
    
    (assert
        (and
            (not (= x0y2 x0y3))
            (not (= x0y2 x1y2))
            (not (= x0y2 x1y3))
            (not (= x0y3 x1y2))
            (not (= x0y3 x1y3))
            (not (= x1y2 x1y3))))
    
    (assert
        (and
            (not (= x2y0 x2y1))
            (not (= x2y0 x3y0))
            (not (= x2y0 x3y1))
            (not (= x2y1 x3y0))
            (not (= x2y1 x3y1))
            (not (= x3y0 x3y1))))

    (assert
        (and
            (not (= x2y2 x2y3))
            (not (= x2y2 x3y2))
            (not (= x2y2 x3y3))
            (not (= x2y3 x3y2))
            (not (= x2y3 x3y3))
            (not (= x3y2 x3y3))))

    (assert
        (and
            (= x0y0 1)
            (not (= x0y1 x0y0))
            (not (= x0y2 x0y1))
            (not (= x0y2 x0y0))
            (= x0y3 4)))
    
    (assert
        (and
            (= x1y0 3)
            (not (= x1y1 x0y0))
            (not (= x1y1 x0y1))
            (= x1y2 1)
            (not (= x1y3 x1y2))
            (not (= x1y3 x1y1))
            (not (= x1y3 x1y0))
            (not (= x1y3 x0y3))))

     (assert
        (and
            (not (= x2y0 x1y0))
            (not (= x2y0 x0y0))
            (= x2y1 1)
            (not (= x2y2 x2y1))
            (not (= x2y2 x2y0))
            (not (= x2y2 x1y2))
            (not (= x2y2 x0y2))
            (= x2y3 3)))
    (assert
        (and
            (not (= x3y0 x2y0))
            (not (= x3y0 x1y0))
            (not (= x3y0 x0y0))
            
            (not (= x3y1 x3y0))
            (not (= x3y1 x2y1))
            (not (= x3y1 x1y1))
            (not (= x3y1 x0y1))
            
            (= x3y2 2)
            
            (not (= x3y3 x3y2))
            (not (= x3y3 x3y1))
            (not (= x3y3 x3y0))
            (not (= x3y3 x2y3))
            (not (= x3y3 x1y3))
            (not (= x3y3 x0y3))))
    (assert
        (and
            (and (> x0y0 0) (< x0y0 5))
            (and (> x0y1 0) (< x0y1 5))
            (and (> x0y2 0) (< x0y2 5))
            (and (> x0y3 0) (< x0y3 5))
            (and (> x1y0 0) (< x1y0 5))
            (and (> x1y1 0) (< x1y1 5))
            (and (> x1y2 0) (< x1y2 5))
            (and (> x1y3 0) (< x1y3 5))
            (and (> x2y0 0) (< x2y0 5))
            (and (> x2y1 0) (< x2y1 5))
            (and (> x2y2 0) (< x2y2 5))
            (and (> x2y3 0) (< x2y3 5))
            (and (> x3y0 0) (< x3y0 5))
            (and (> x3y1 0) (< x3y1 5))
            (and (> x3y2 0) (< x3y2 5))
            (and (> x3y3 0) (< x3y3 5))))
    (check-sat)
    (get-model)
(pop)