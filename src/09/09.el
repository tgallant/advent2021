;; https://adventofcode.com/2021/day/9

(require 'ert)

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

(defun is-valid-basin-member (val lowest)
  (if (not (eq val nil))
      (and (not (eq val 9))
           (> val lowest))))

(defun measure-basin (x-value y-value levels)
  (let ((acc '()))
    (defun measure-basin-inner (x y)
      (let* ((val (at-index x (at-index y levels)))
             (up (at-index x (at-index (- y 1) levels)))
             (down (at-index x (at-index (+ y 1) levels)))
             (left (at-index (- x 1) (at-index y levels)))
             (right (at-index (+ x 1) (at-index y levels))))
        (add-to-list 'acc `(,x . ,y))
        (if (is-valid-basin-member up val)
            (measure-basin-inner x (- y 1)))
        (if (is-valid-basin-member down val)
            (measure-basin-inner x (+ y 1)))
        (if (is-valid-basin-member left val)
            (measure-basin-inner (- x 1) y))
        (if (is-valid-basin-member right val)
            (measure-basin-inner (+ x 1) y))))
    (measure-basin-inner x-value  y-value)
    acc))

(defun find-basins (levels)
  (loop with acc for y from 0 to (- (length levels) 1)
        do (loop for x from 0 to (- (length (nth 0 levels)) 1)
                 if (check-if-low-point x y levels)
                 do (push (measure-basin x y levels) acc))
        finally return acc))

(defun calculate-basin-sizes (levels)
  (let* ((basins (find-basins levels))
         (sizes (mapcar 'length basins))
         (sorted (sort sizes #'>))
         (top-three (subseq sorted 0 3)))
    (reduce '* top-three)))

(defun line-to-levels (line)
  (mapcar 'string-to-number (split-string line "" t)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-calculate-basin-sizes (file)
  (let* ((lines (read-lines file))
         (levels (mapcar 'line-to-levels lines)))
    (calculate-basin-sizes levels)))

(ert-deftest 09-sum-test-levels-test-data ()
  (let* ((lines (read-lines "./09.test.txt"))
         (levels (mapcar 'line-to-levels lines)))
    (should (= (sum-risk-levels levels) 15))))

(ert-deftest 09-sum-test-levels-input-data ()
  (let* ((lines (read-lines "./09.input.txt"))
         (levels (mapcar 'line-to-levels lines)))
    (should (= (sum-risk-levels levels) 514))))

(ert-deftest 09-calculate-basin-sizes-test-data ()
  (should (= (test-calculate-basin-sizes "./09.test.txt") 1134)))

(ert-deftest 09-calculate-basin-sizes-input-data ()
  (should (= (test-calculate-basin-sizes "./09.input.txt") 1103130)))

(ert "09")
