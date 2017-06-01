(defun rgr7(&optional i res)
  (when (eq i nil) (setq i 20))
  (setq res
        (if (< i 11)
            (if (or (eq i 1) (eq i 10))
                1
                (sin (- (rgr7 (- i 1)) (cos i))))
            (cos (+ (rgr7 (- i 1)) (sin i)))))
  (when (eq i 10)
    (rgr7 (- i 1)))
  (if (< i 11)
      (format t "i=~d;~3T F=~d.~%" i res)
      (format t "i=~d;~3T F=~d.~%" i res (- i 1) i))
  (return-from rgr7 res))
