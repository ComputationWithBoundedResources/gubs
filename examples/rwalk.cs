(>= (f (s (var x)))
    (+ 
      1
      (* (/ 2 3) (f (var x))) 
      (* (/ 1 3) (f (s (s (var x)))))
    )
)
(>= (f (var x)) (var x))
(>= (s (var x)) (var x))

