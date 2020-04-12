(declare-const x Real)
(declare-const y Real)
(declare-const z Real)
(declare-const w Real)

(assert (= x 3))
(assert (= (* x y) (- z w)))
(assert (not (= y 0)))
(assert (not (= z 0)))
(assert (not (= w 0)))

(assert (not (= y 1)))
(assert (not (= z -1)))
(assert (not (= w -4)))


(check-sat)
(get-model)