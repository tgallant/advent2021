; https://adventofcode.com/2021/day/7

(defun difference (x y)
  (let ((minv (min x y))
        (maxv (max x y)))
    (- maxv minv)))

(defun difference-v2 (x y)
  (let* ((amt (difference x y))
         (seq (number-sequence 1 amt)))
    (reduce '+ seq)))

(defun calculate-crab-position-alignment (positions)
  (defun calculate-fuel-usage (pos)
    (defun sum-difference (acc cur)
      (+ acc (difference cur pos)))
    (reduce 'sum-difference positions :initial-value 0))
  (let* ((min-pos (apply 'min positions))
         (max-pos (apply 'max positions))
         (pos-seq (number-sequence min-pos max-pos)))
    (apply 'min (mapcar 'calculate-fuel-usage pos-seq))))

(defun calculate-crab-position-alignment-v2 (positions)
  (defun calculate-fuel-usage (pos)
    (defun sum-difference (acc cur)
      (+ acc (difference-v2 cur pos)))
    (reduce 'sum-difference positions :initial-value 0))
  (let* ((min-pos (apply 'min positions))
         (max-pos (apply 'max positions))
         (pos-seq (number-sequence min-pos max-pos)))
    (apply 'min (mapcar 'calculate-fuel-usage pos-seq))))

(defun line-to-positions (line)
  (let ((strings (split-string line "," t)))
    (mapcar 'string-to-number strings)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 07-calculate-crab-position-alignment-test-data ()
  (let* ((lines (read-lines "./07.test.txt"))
         (positions (line-to-positions (car lines))))
    (should (= (calculate-crab-position-alignment positions) 37))))

(ert-deftest 07-calculate-crab-position-alignment-input-data ()
  (let* ((lines (read-lines "./07.input.txt"))
         (positions (line-to-positions (car lines))))
    (should (= (calculate-crab-position-alignment positions) 342534))))

(ert-deftest 07-calculate-crab-position-alignment-v2-test-data ()
  (let* ((lines (read-lines "./07.test.txt"))
         (positions (line-to-positions (car lines))))
    (should (= (calculate-crab-position-alignment-v2 positions) 168))))

(ert-deftest 07-calculate-crab-position-alignment-v2-input-data ()
  (let* ((lines (read-lines "./07.input.txt"))
         (positions (line-to-positions (car lines))))
    (should (= (calculate-crab-position-alignment-v2 positions) 94004208))))

(ert "07")
