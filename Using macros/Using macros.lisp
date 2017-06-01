(defun simplep (n)
  (block myloop
    (loop for x from 2 to n
          when (and (eq 0 (mod n x))
                    (not (eq x n)))
          do (return-from myloop nil))
    (return-from myloop t)))

(defmacro func (n1 n2)
  (loop for x from n1 to n2
        when (eq 0 (mod x 2))
        collect (list x
                      (block mybl
                        (loop for i from 2 to x
                              when (simplep i)
                              do (loop for j from 2 to x
                                       when (and (simplep j)
                                                 (eq x (+ i j)))
                                       do (return-from mybl (list i j))))
                        (return-from mybl nil)))))
(defun lab6 (n1 n2)
  (macroexpand `(func ,n1 ,n2)))
