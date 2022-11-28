;; https://adventofcode.com/2021/day/1

(require 'ert)

(defun compare-depths (acc cur)
  (let ((prv (car acc))
        (cnt (cdr acc)))
    (cond ((eq prv 'nil)
           (cons cur cnt))
          ((> cur prv)
           (cons cur (+ cnt 1)))
          ((<= cur prv)
           (cons cur cnt)))))

(defun count-increases (num-list)
  (defun count-increases-inner ()
    (cl-reduce 'compare-depths num-list :initial-value '(nil . 0)))
  (cdr (count-increases-inner)))

(defun count-increases-window (num-list window)
  (defun build-window (acc cur)
    (let* ((w (alist-get 'win acc))
           (b (alist-get 'buf acc))
           (n (cons cur (mapcar (lambda (x) (+ x cur)) b))))
      (cond ((= (length n) window)
             `((win . ,(cons (car (last n)) w))
               (buf . ,(butlast n 1))))
            ((< (length n) window)
             `((win . ,w) (buf . ,n))))))
  (defun window ()
    (cl-reduce 'build-window num-list :initial-value '((win . nil) (buf . nil))))
  (count-increases (reverse (alist-get 'win (window)))))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest count-increases-test-data ()
  (let* ((lines (read-lines "./01.test.txt"))
         (num-list (mapcar 'string-to-number lines)))
    (should (= (count-increases num-list) 7))))

(ert-deftest count-increases-input-data ()
  (let* ((lines (read-lines "./01.input.txt"))
         (num-list (mapcar 'string-to-number lines)))
    (should (= (count-increases num-list) 1451))))

(ert-deftest count-increases-window-test-data ()
  (let* ((lines (read-lines "./01.test.txt"))
         (num-list (mapcar 'string-to-number lines)))
    (should (= (count-increases-window num-list 3) 5))))

(ert-deftest count-increases-window-input-data ()
  (let* ((lines (read-lines "./01.input.txt"))
         (num-list (mapcar 'string-to-number lines)))
    (should (= (count-increases-window num-list 3) 1395))))

(ert "count-increases")
