;; https://adventofcode.com/2021/day/13

(require 'ert)

(defun transform-value (val fold-value)
  (let* ((max-val (max val fold-value))
         (min-val (min val fold-value))
         (distance-to-fold (- max-val min-val))
         (total-distance (* 2 distance-to-fold)))
    (- val total-distance)))

(defun transform-point (point fold-axis fold-value)
  (cond ((equal fold-axis 'x)
         (list (transform-value (nth 0 point) fold-value) (nth 1 point)))
        ((equal fold-axis 'y)
         (list (nth 0 point) (transform-value (nth 1 point) fold-value)))))

(defun split-points (points fold)
  (let* ((fold-type (car fold))
         (fold-point (cdr fold))
         (fold-axis (car fold-point))
         (fold-value (cdr fold-point)))
    (defun sort-point (acc cur)
      (let ((left (car acc))
            (right (cdr acc)))
        (cond ((equal fold-axis 'x)
               (if (> (nth 0 cur) fold-value)
                   (cons (cons (transform-point cur fold-axis fold-value) left) right)
                 (cons left (cons cur right))))
              ((equal fold-axis 'y)
               (if (> (nth 1 cur) fold-value)
                   (cons (cons (transform-point cur fold-axis fold-value) left) right)
                 (cons left (cons cur right)))))))
    (cl-reduce 'sort-point points :initial-value '())))

(defun merge-point (acc cur)
  (if (member cur acc) acc
    (cons cur acc)))

(defun merge-points (split)
  (cl-reduce 'merge-point (car split) :initial-value (cdr split)))

(defun split-and-merge-points (acc cur)
  (merge-points (split-points acc cur)))

(defun fold-points (points folds)
  (cl-reduce 'split-and-merge-points folds :initial-value points))

(defun count-points-after-folds (instructions num-folds)
  (let ((points (car instructions))
        (folds (seq-subseq (reverse (cdr instructions)) 0 num-folds)))
    (length (fold-points points folds))))

(defun display-points (points)
  (let ((max-x (apply 'max (mapcar 'first points)))
        (min-x (apply 'min (mapcar 'first points)))
        (max-y (apply 'max (mapcar 'cl-second points))))
    (defun display-row (row)
      (print (cl-loop for x from min-x to max-x
               if (member (list x row) points)
                 concat "X"
               else
                 concat ".")))
    (mapc 'display-row (number-sequence 0 max-y))))

(defun display-points-after-folds (instructions num-folds)
  (let ((points (car instructions))
        (folds (seq-subseq (reverse (cdr instructions)) 0 num-folds)))
    (display-points (fold-points points folds))))

(defun make-fold-point (s)
  (let ((parts (split-string s "=" t)))
    (cons (intern (nth 0 parts)) (string-to-number (nth 1 parts)))))

(defun make-fold (s)
  (let* ((parts (split-string s " " t))
         (fold (intern (nth 1 parts)))
         (fold-point (make-fold-point (nth 2 parts))))
    (cons fold fold-point)))

(defun make-point (s)
  (mapcar 'string-to-number (split-string s "," t)))

(defun line-to-instruction (acc cur)
  (let ((points (car acc))
        (folds (cdr acc)))
    (if (string-prefix-p "fold" cur)
        (cons points (cons (make-fold cur) folds))
      (cons (cons (make-point cur) points) folds))))

(defun lines-to-instructions (lines)
  (cl-reduce 'line-to-instruction lines :initial-value '()))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-count-points-after-folds (file &optional num-folds)
  (let* ((lines (read-lines file))
         (instructions (lines-to-instructions lines)))
    (count-points-after-folds instructions num-folds)))

(defun test-display-points-after-folds (file &optional num-folds)
  (let* ((lines (read-lines file))
         (instructions (lines-to-instructions lines)))
    (display-points-after-folds instructions num-folds)))

(ert-deftest 13-count-points-after-folds-test-data ()
  (should (= (test-count-points-after-folds "./13.test.txt" 1) 17)))

(ert-deftest 13-count-points-after-folds-input-data ()
  (should (= (test-count-points-after-folds "./13.input.txt" 1) 743)))

(ert "13")
