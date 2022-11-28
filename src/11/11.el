;; https://adventofcode.com/2021/day/11

(require 'ert)

(defun at-index (idx lst)
  (if (>= idx 0)
      (nth idx lst)))

(defun incr-num-by-1 (num)
  (+ num 1))

(defun incr-cols-by-1 (row)
  (mapcar 'incr-num-by-1 row))

(defun incr-by-1 (rows)
  (mapcar 'incr-cols-by-1 rows))

(defun reset-flashed-num (num)
  (if (> num 9) 0
    num))

(defun reset-flashed-cols (row)
  (mapcar 'reset-flashed-num row))

(defun reset-flashed (rows)
  (mapcar 'reset-flashed-cols rows))

(defun val-or-10 (val)
  (if (> val 10) 10
    val))

(defun next-point-value (acc cur)
  (if (eq cur 10)
      (+ 1 acc)
    acc))

(defun check-point (x y rows)
  (let* ((val (at-index x (at-index y rows)))
         (up (at-index x (at-index (- y 1) rows)))
         (down (at-index x (at-index (+ y 1) rows)))
         (left (at-index (- x 1) (at-index y rows)))
         (right (at-index (+ x 1) (at-index y rows)))
         (up-left (at-index (- x 1) (at-index (- y 1) rows)))
         (up-right (at-index (+ x 1) (at-index (- y 1) rows)))
         (down-left (at-index (- x 1) (at-index (+ y 1) rows)))
         (down-right (at-index (+ x 1) (at-index (+ y 1) rows)))
         (neighbors (list val up down left right up-left up-right down-left down-right)))
    (reduce 'next-point-value neighbors :initial-value val)))

(defun flash-loop (rows)
  (loop with new-flashes = 0 for y from 0 to (- (length rows) 1)
        collect (loop with cur with val with next for x from 0 to (- (length (nth 0 rows)) 1)
                      do (setq val (at-index x (at-index y rows)))
                      do (setq cur (check-point x y rows))
                      do (setq next (or (if (and (< val 10) (>= cur 10)) 10) cur))
                      collect next
                      if (= 10 next)
                        do (incf new-flashes))
        into next
        finally return (cons next new-flashes)))

(defun process-flashes (rows)
  (let* ((result (flash-loop rows))
         (state (car result))
         (new-flashes (cdr result)))
    (if (= 0 new-flashes)
        state
      (process-flashes state))))

(defun count-flashed (rows)
  (loop for y from 0 to (- (length rows) 1)
        sum (loop for x from 0 to (- (length (nth 0 rows)) 1)
                      count (> (at-index x (at-index y rows)) 9))))

(defun process-step (rows)
  (let* ((incremented (incr-by-1 rows))
         (flashed (process-flashes incremented))
         (count (count-flashed flashed)))
    (cons (reset-flashed flashed) count)))

(defun count-flashes (steps rows)
  (loop with state = rows with count = 0 with step-result for step from 1 to steps
        do (setq step-result (process-step state))
        do (setq state (car step-result))
        do (setq count (+ count (cdr step-result)))
        finally return count))

(defun detect-simultaneous-flash (steps rows)
  (let* ((row-count (length rows))
         (col-count (length (nth 0 rows)))
         (total (* row-count col-count)))
    (loop with state = rows with count = 0 with step-result for step from 1 to steps
          do (setq step-result (process-step state))
          do (setq state (car step-result))
          do (incf count)
          if (= (cdr step-result) total)
            return step)))

(defun line-to-list (line)
  (mapcar 'string-to-number (split-string line "" t)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-count-flashes (steps file)
  (let* ((lines (read-lines file))
         (split-lines (mapcar 'line-to-list lines)))
    (count-flashes steps split-lines)))

(defun test-detect-simultaneous-flash (steps file)
  (let* ((lines (read-lines file))
         (split-lines (mapcar 'line-to-list lines)))
    (detect-simultaneous-flash steps split-lines)))

(ert-deftest 11-count-flashes-small-test-data ()
  (should (= (test-count-flashes 1 "./11.test.small.txt") 9)))

(ert-deftest 11-count-flashes-test-data ()
  (should (= (test-count-flashes 10 "./11.test.txt") 204)))

(ert-deftest 11-count-flashes-100-test-data ()
  (should (= (test-count-flashes 100 "./11.test.txt") 1656)))

(ert-deftest 11-count-flashes-input-data ()
  (should (= (test-count-flashes 100 "./11.input.txt") 1642)))

(ert-deftest 11-detect-simultaneous-flash-test-data ()
  (should (= (test-detect-simultaneous-flash 200 "./11.test.txt") 195)))

(ert-deftest 11-detect-simultaneous-flash-input-data ()
  (should (= (test-detect-simultaneous-flash 500 "./11.input.txt") 320)))

(ert "11")
