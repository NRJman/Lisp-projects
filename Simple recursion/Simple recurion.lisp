(defun good (mylist)
  (if (null (cddr mylist))
      mylist
      (good (cdr mylist))))
(defun job (mylist)
   (cons (car mylist) (list (cadr mylist) (caddr mylist))))
(defun goodjob (mylist)
  (append (job mylist) (good mylist)))
