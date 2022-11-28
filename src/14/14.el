;; https://adventofcode.com/2021/day/14

(require 'ert)

(defun make-pairs (lst)
  (defun pair (idx)
    (list (nth idx lst) (nth (+ 1 idx) lst)))
  (mapcar 'pair (number-sequence 0 (- (length lst) 1))))

(defun do-polymerization (template instructions steps)
  (defun do-step (acc cur)
    (cl-loop with match for (a b) in (make-pairs acc)
             do (setf match (gethash (concat a b) instructions))
             collect a
             if match collect match))
  (cl-reduce 'do-step (number-sequence 1 steps) :initial-value template))

(defun count-occurrences (lst)
  (defun count-item (acc cur)
    (let ((val (gethash cur acc)))
      (if val
          (puthash cur (+ val 1) acc)
        (puthash cur 1 acc))
      acc))
  (cl-reduce 'count-item lst :initial-value (make-hash-table :test 'equal)))

(defun count-pairs (lst)
  (defun count-item (acc cur)
    (let ((val (gethash cur acc)))
      (if (second cur)
          (if val
              (puthash cur (+ val 1) acc)
            (puthash cur 1 acc)))
      acc))
  (cl-reduce 'count-item lst :initial-value (make-hash-table :test 'equal)))

(defun incby (v b h)
  (let ((val (gethash v h)))
    (if val
        (puthash v (+ val b) h)
      (puthash v b h))))

(defun kvpairs (hash)
  (defun kv (k)
    (cons k (gethash k hash)))
  (mapcar 'kv (hash-table-keys hash)))

(defun count-occurrences-v2 (template instructions steps)
  (let ((elhash (count-occurrences template))
        (pairhash (count-pairs (make-pairs template))))
    (defun do-step (p)
      (let* ((k (car p))
             (v (cdr p))
             (next (gethash (apply 'concat k) instructions)))
        (incby next v elhash)
        (incby k (* -1 v) pairhash)
        (incby (list (first k) next) v pairhash)
        (incby (list next (second k)) v pairhash)))
    (cl-loop repeat steps do (mapc 'do-step (kvpairs pairhash)))
    elhash))

(defun calculate-polymer-solution (template instructions steps)
  (let* ((result (do-polymerization template instructions steps))
         (occurrences (count-occurrences result))
         (values (hash-table-values occurrences)))
    (- (apply 'max values) (apply 'min values))))

(defun calculate-polymer-solution-v2 (template instructions steps)
  (let* ((occurrences (count-occurrences-v2 template instructions steps))
         (values (hash-table-values occurrences)))
    (- (apply 'max values) (apply 'min values))))

(defun make-instructions (lines)
  (defun line-to-instruction (acc cur)
    (let ((parts (split-string cur " -> " t)))
      (puthash (nth 0 parts) (nth 1 parts) acc)
      acc))
  (cl-reduce 'line-to-instruction lines
             :initial-value (make-hash-table :test 'equal)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-calculate-polymer-solution (file steps)
  (let* ((lines (read-lines file))
         (template (split-string (car lines) "" t))
         (instructions (make-instructions (cdr lines))))
    (calculate-polymer-solution template instructions steps)))

(defun test-calculate-polymer-solution-v2 (file steps)
  (let* ((lines (read-lines file))
         (template (split-string (car lines) "" t))
         (instructions (make-instructions (cdr lines))))
    (calculate-polymer-solution-v2 template instructions steps)))

(ert-deftest 14-calculate-polymer-solution-test-data ()
  (should (= (test-calculate-polymer-solution "./14.test.txt" 10) 1588)))

(ert-deftest 14-calculate-polymer-solution-input-data ()
  (should (= (test-calculate-polymer-solution "./14.input.txt" 10) 2112)))

(ert-deftest 14-calculate-polymer-solution-v2-test-data ()
  (should (= (test-calculate-polymer-solution-v2 "./14.test.txt" 10) 1588)))

(ert-deftest 14-calculate-polymer-solution-v2-input-data ()
  (should (= (test-calculate-polymer-solution-v2 "./14.input.txt" 10) 2112)))

(ert-deftest 14-calculate-polymer-solution-v2-input-data-40 ()
  (should (= (test-calculate-polymer-solution-v2 "./14.input.txt" 40) 3243771149914)))

(ert "14")
