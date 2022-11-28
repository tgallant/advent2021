;; https://adventofcode.com/2021/day/2

(require 'ert)

(defun execute-instr (acc cur)
  (let ((x (car acc))
        (y (cdr acc))
        (n (car cur))
        (d (cdr cur)))
    (cond ((string-equal n "forward")
           (cons (+ x d) y))
          ((string-equal n "up")
           (cons x (- y d)))
          ((string-equal n "down")
           (cons x (+ y d))))))

(defun navigate-submarine (instr)
  (cl-reduce 'execute-instr instr :initial-value '(0 . 0)))

(defun coords-product (instr)
  (let ((c (navigate-submarine instr)))
    (* (car c) (cdr c))))

(defun make-params (x y a)
  `((x-axis . ,x)
    (y-axis . ,y)
    (aim . ,a)))

(defun execute-instr-v2 (acc cur)
  (let ((x (alist-get 'x-axis acc))
        (y (alist-get 'y-axis acc))
        (a (alist-get 'aim acc))
        (n (car cur))
        (d (cdr cur)))
    (cond ((string-equal n "forward")
           (make-params (+ x d) (+ y (* d a)) a))
          ((string-equal n "up")
           (make-params x y (- a d)))
          ((string-equal n "down")
           (make-params x y (+ a d))))))

(defun navigate-submarine-v2 (instr)
  (cl-reduce 'execute-instr-v2 instr :initial-value (make-params 0 0 0)))

(defun coords-product-v2 (instr)
  (let* ((c (navigate-submarine-v2 instr))
         (x (alist-get 'x-axis c))
         (y (alist-get 'y-axis c)))
    (* x y)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun line-to-instr (line)
  (let ((parts (split-string line " " t)))
    (cons (car parts) (string-to-number (cadr parts)))))

(ert-deftest navigate-submarine-test-data ()
  (let* ((lines (read-lines "./02.test.txt"))
         (instr (mapcar 'line-to-instr lines)))
    (should (= (coords-product instr) 150))))

(ert-deftest navigate-submarine-input-data ()
  (let* ((lines (read-lines "./02.input.txt"))
         (instr (mapcar 'line-to-instr lines)))
    (should (= (coords-product instr) 1604850))))

(ert-deftest navigate-submarine-v2-test-data ()
  (let* ((lines (read-lines "./02.test.txt"))
         (instr (mapcar 'line-to-instr lines)))
    (should (= (coords-product-v2 instr) 900))))

(ert-deftest navigate-submarine-v2-input-data ()
  (let* ((lines (read-lines "./02.input.txt"))
         (instr (mapcar 'line-to-instr lines)))
    (should (= (coords-product-v2 instr) 1685186100))))

(ert "navigate-submarine")
