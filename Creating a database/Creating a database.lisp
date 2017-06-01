(defun list-all (stream &key (pausep t) (searchp nil))
  (if (not (null stream))
      (block printing
        (prog2
            (if searchp
                (format t "List of all contacts we've found:~% Id~5TName~25TPhone~45TAddress~%")
                (format t "List of all contacts:~% Id~5TName~25TPhone~45TAddress~%"))
            (do ((number 1 (+ number 1)) (item (car stream) (nth number stream)))
                ((null item) (return-from printing))
              (format t "~3d. ~a~25T~a~45T~a~%" number (car item) (cadr item) (caddr item)))))
      (if searchp
          (format t "We haven't found any record.~%")
          (format t "Book is empty.~%")))
  (when pausep
    (format t "Press any key to return to previous menu.")
    (read-line)))

(defun get-option (&key (option "option") (searchp nil))
  (format t "Enter ~a:~%>>> " option)
  (when searchp
    (return-from get-option (read-line)))
  (setq answer (read))
  (if (integerp answer)
      answer
      nil))

(defun contact-input ()                 
  (format t "Enter name:~%")
  (setq result (list  (read-line)))
  (format t "Enter phone number:~%")
  (setf result (append result (list(read-line))))
  (format t "Enter adress:~%")
  (setf result (append result (list (read-line)))))

(defun contact-add (stream)             
  (let*((contact (contact-input)))
    (cons contact stream)))

(defun contact-delete(stream)           
  (list-all stream :pausep nil)
  (delete
   (nth (1- (get-option :option "an Id of record You want to remove from book"))
               stream)
          stream
          :count 1))

(defun break-string (seq &optional (delimeter #\Space)) ;works
  (let ((pos (position delimeter seq)))
    (if (null pos)
        (list seq)
        (cons (subseq seq 0 pos)
              (break-string (subseq seq (1+ pos)) delimeter)))))

(defun substrp (seq1 seq2 &key (start1 0) (start2 0))             
  (let* ((x start1)
         (y start2))
    (when (equal seq1 "")
      (return-from substrp y))
    (block cycle
      (loop when (eq x (length seq1))
            do (return-from cycle (- y x))
            when (eq y (length seq2))
            do (return-from cycle nil)
            if (equal (char seq1 x)
                      (char seq2 y))
            do (progn (setf x (1+ x))
                      (setf y (1+ y)))
            else
            do (progn (setf y (1+ y))
                      (setf y (- y x))
                      (setf x start1))))))

(defun maskedp(mask what)              
  (setq stream (break-string mask #\*))
  (setq oldpos 0)
  (if(equal (first stream) "")
     (setq flag t)
     (setq flag nil))
  (block cycle
    (loop for item in stream
          do(setq newpos (substrp item what :start2 oldpos))
          if flag
          do(if(not(null newpos))
               (setf oldpos (+ (length item) newpos))
               (return-from cycle nil))
          else
          do(if (and (not (null newpos))
                     (eq newpos 0))
                (prog2
                    (setf flag t)
                    (setf oldpos (+ (length item) newpos)))
                (return-from cycle nil)))
    (when (not(equal (car(last stream)) ""))
      (if (eq 0 (substrp
                 (reverse (car (last stream)))
                 (reverse what)))
          (return-from cycle t)
          (return-from cycle nil)))
    t))


(defun seek (what where &key (test "first"))
  (setq res nil)
  (loop named l1
        for item in where
        when (or (and (equal test "first")
                      (equal what (string-downcase (first item))))
                 (equal what (string-downcase (second item))))
        do (setq res (append res (list item))))
  res)


(defun seek-by(what where)             
  (cond ((equal what "name")
         (setq res
               (seek
                (string-downcase (get-option :option "a name" :searchp t))
                where)))
        ((equal what "phone")
         (setq res
               (seek
                (string-downcase (get-option :option "a phone" :searchp t))
                where :test "second")))
         ((equal what "mask")
          (progn (setq mask (get-option :option "a name-mask(asterisk means any number of letters)" :searchp t))
                 (setq res ())
                 (loop named l1
                       for item in where
                       do (setq cond (maskedp
                                      (string-downcase mask)
                                      (string-downcase (first item))))
                       when cond
                       do (setq res (append res (list item)))))))
  (list-all res :searchp t))
         
(defun menu-main()                     
  (format t "1. List all~%")
  (format t "2. Add new contact~%")
  (format t "3. Delete contact~%")
  (format t "4. List by mask~%")
  (format t "5. Search by name~%")
  (format t "6. Search by phone~%")
  (format t "7. Exit~%"))


(defun response-main(stream)           
  (loop
     do (let ((answer (prog1 (get-option))))
          (cond ((eq answer 1)(prog1 (list-all stream)(menu-main)))
                ((eq answer 2)(setf stream (prog1 (contact-add stream)(menu-main))))
                ((eq answer 3)(setf stream (prog1 (contact-delete stream)(menu-main))))
                ((eq answer 4)(seek-by "mask" stream) (menu-main))
                ((eq answer 5)(seek-by "name" stream)(menu-main))
                ((eq answer 6)(seek-by "phone" stream)(menu-main))
                ((eq answer 7) (return-from response-main stream))
                (t (progn (menu-main)
                          (format t "Please choose one of the prefered options above by entering number.~%")))))))


(defun initialise (file)             
  (let* ((tmp nil))
    (with-open-file
        (stream file :direction :input
                     :if-does-not-exist nil)
      (when (null stream)
        (return-from initialise nil))
      (do ((item (read stream nil nil) (read stream nil nil)))
          ((null item) (return-from initialise tmp))
        (setf tmp (append tmp (list item)))))))



(defun save(file stream)               
  (with-open-file (filestream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (loop :for item :in stream
          :do (format filestream "~s" item))))

(defun database()                          
  (let* ((File (string "database.db"))
         (stream (initialise File)))
    (menu-main)
    (save File (response-main stream))))
