;; https://adventofcode.com/2021/day/9

(defun at-index (idx lst)
  (if (>= idx 0)
      (nth idx lst)))

(defun check-if-low-point (x-value y-value levels)
  (let ((val (at-index x-value (at-index y-value levels)))
        (up (at-index x-value (at-index (- y-value 1) levels)))
        (down (at-index x-value (at-index (+ y-value 1) levels)))
        (left (at-index (- x-value 1) (at-index y-value levels)))
        (right (at-index (+ x-value 1) (at-index y-value levels))))
    (if (and (< val (or up 10))
             (< val (or down 10))
             (< val (or left 10))
             (< val (or right 10)))
          val)))

(defun find-low-points (levels)
  (loop for y from 0 to (- (length levels) 1)
        collect (loop for x from 0 to (- (length (nth 0 levels)) 1)
                 collect (check-if-low-point x y levels))))

(defun format-risk-level (val)
  (if (eq val nil)
      0
    (+ val 1)))

(defun sum-risk-levels (levels)
  (let ((low-points (flatten-list (find-low-points levels))))
    (reduce '+ (mapcar 'format-risk-level low-points))))

(defun line-to-levels (line)
  (mapcar 'string-to-number (split-string line "" t)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 09-sum-test-levels-test-data ()
  (let* ((lines (read-lines "./09.test.txt"))
         (levels (mapcar 'line-to-levels lines)))
    (should (= (sum-risk-levels levels) 15))))

(ert-deftest 09-sum-test-levels-input-data ()
  (let* ((lines (read-lines "./09.input.txt"))
         (levels (mapcar 'line-to-levels lines)))
    (should (= (sum-risk-levels levels) 514))))

(ert "09")
