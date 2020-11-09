(declare-const a_49 Int)
(declare-const a_48 Int)
(declare-const a_47 Int)
(assert (= 0 (ite (and (= 1 0) (= a_47 0)) 0 
                            (ite (and (= 1 1) (= a_47 1)) 1 
                            (ite (and (= 1 -1) (= a_47 -1)) -1 
                            (ite (and (= 1 1) (= a_47 0)) 1 
                            (ite (and (= 1 0) (= a_47 1)) 1
                            (ite (and (= 1 -1) (= a_47 0)) -1 
                            (ite (and (= 1 0) (= a_47 -1)) -1 
                            (ite (and (= 1 1) (= a_47 -1)) (ite (> 0 a_48) 1 (ite (< 0 a_48) -1 2)) 
                            (ite (and (= 1 -1) (= a_47 1)) (ite (< 0 a_48) 1 (ite (> 0 a_48) -1 2))
                            2)))))))))))
(assert (= 0 (+ (ite (>= 0 a_48) 0 a_48) (ite (= 1 0) 0
                           (ite (= a_47 0) 0 
                           (ite (and (= 1 1) (= a_47 -1)) 0 
                           (ite (and (= 1 -1) (= a_47 1)) 0 
                           1)))))))
(assert (>= 53 (- (- (+ (ite (>= 0 a_48) 0 a_48) (ite (= 1 0) 0
                           (ite (= a_47 0) 0 
                           (ite (and (= 1 1) (= a_47 -1)) 0 
                           (ite (and (= 1 -1) (= a_47 1)) 0 
                           1))))) (ite (>= -53 (- a_48 a_49)) -53 (- a_48 a_49))) (ite (= -53 (- a_48 a_49)) 1 0))))
(check-sat)
(get-model)
