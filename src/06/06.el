; https://adventofcode.com/2021/day/6

(require 'ert)

(defun simulate-day (acc cur)
  (print (format "day %s" cur))
  (cl-loop with next-val for fish in acc
        do (setq next-val (- fish 1))
        if (= next-val -1) collect 6 and collect 8
        else collect next-val))

(defun simulate-fish-growth (fish days)
  (let ((days-list (cl-loop for x from 1 to days collect x)))
    (cl-reduce 'simulate-day days-list :initial-value fish)))

(defun line-to-fish (line)
  (let ((parts (split-string line ",")))
    (mapcar 'string-to-number parts)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 06-simulate-fish-growth-test-data ()
  (let* ((lines (read-lines "./06.test.txt"))
         (fish (line-to-fish (car lines))))
    (should (= (length (simulate-fish-growth fish 80)) 5934))))

(ert-deftest 06-simulate-fish-growth-input-data ()
  (let* ((lines (read-lines "./06.input.txt"))
         (fish (line-to-fish (car lines))))
    (should (= (length (simulate-fish-growth fish 80)) 352872))))

(ert-deftest 06-simulate-fish-growth-256-test-data ()
  (let* ((lines (read-lines "./06.test.txt"))
         (fish (line-to-fish (car lines))))
    (should (= (length (simulate-fish-growth fish 256)) 26984457539))))

(ert-deftest 06-simulate-fish-growth-256-input-data ()
  (let* ((lines (read-lines "./06.input.txt"))
         (fish (line-to-fish (car lines))))
    (should (= (length (simulate-fish-growth fish 256)) 352872))))

(defun 06-simulate-fish-growth-256-input-data ()
  (let* ((lines (read-lines "./06.input.txt"))
         (fish (line-to-fish (car lines))))
    (length (simulate-fish-growth fish 256))))

(ert "06-simulate-fish-growth-256-input-data")
