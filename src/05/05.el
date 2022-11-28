; https://adventofcode.com/2021/day/5

(require 'ert)
(require 'subr-x)

(defun string-to-coord (coord-str)
  (let* ((parts (split-string coord-str ","))
         (start (string-to-number (nth 0 parts)))
         (end (string-to-number (nth 1 parts))))
    `(,start . ,end)))

(defun point-to-string (point)
  (let ((x (number-to-string (car point)))
        (y (number-to-string (cdr point))))
    (string-join (list x y) " -> ")))

(defun make-horizontal-points (start end y)
  (let ((s (min start end))
        (e (max start end)))
    (cl-loop for x from s to e
          collect `(,x . ,y))))

(defun make-vertical-points (start end x)
  (let ((s (min start end))
        (e (max start end)))
    (cl-loop for y from s to e
          collect `(,x . ,y))))

(defun make-diagonal-points (startx starty endx endy)
  (cond ((and (< startx endx) (< starty endy))
         (cl-loop for x from startx to endx
               for y from starty to endy
               collect `(,x . ,y)))
        ((and (< startx endx) (> starty endy))
         (cl-loop for x from startx to endx
               for y from starty downto endy
               collect `(,x . ,y)))
        ((and (> startx endx) (< starty endy))
         (cl-loop for x from startx downto endx
               for y from starty to endy
               collect `(,x . ,y)))
        ((and (> startx endx) (> starty endy))
         (cl-loop for x from startx downto endx
               for y from starty downto endy
               collect `(,x . ,y)))))

(defun get-all-points (coord diagonal)
  (let ((startx (car (alist-get 'start coord)))
        (starty (cdr (alist-get 'start coord)))
        (endx (car (alist-get 'end coord)))
        (endy (cdr (alist-get 'end coord))))
    (cond ((= startx endx)
           (make-vertical-points starty endy startx))
          ((= starty endy)
           (make-horizontal-points startx endx starty))
          ((eq diagonal t)
           (make-diagonal-points startx starty endx endy)))))

(defun make-points-table (coords diagonal)
  (defun log-all-points (acc cur)
    (let ((points (get-all-points cur diagonal)))
      (cl-loop for point in points
            with a = acc
            with point-str
            with count
            do (setq point-str (point-to-string point))
            do (setq count (gethash point-str a 0))
            do (puthash point-str (+ 1 count) a)
            finally return a)))
  (cl-reduce 'log-all-points coords
          :initial-value (make-hash-table :test 'equal)))

(defun detect-overlapping-vents (coords &rest props)
  (let* ((diagonal (plist-get props :diagonal))
         (points-count (make-points-table coords diagonal))
         (overlapping 0))
    (defun count-overlapping (key value)
      (if (> value 1) (setq overlapping (+ 1 overlapping))))
    (maphash 'count-overlapping points-count)
    overlapping))

(defun string-to-coord (coord-str)
  (let* ((parts (split-string coord-str ","))
         (start (string-to-number (nth 0 parts)))
         (end (string-to-number (nth 1 parts))))
    `(,start . ,end)))

(defun line-to-coords (line)
  (let* ((parts (split-string line " -> "))
         (start (string-to-coord (nth 0 parts)))
         (end (string-to-coord (nth 1 parts))))
    `((start . ,start)
      (end . ,end))))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 05-detect-overlapping-vents-test-data ()
  (let* ((lines (read-lines "./05.test.txt"))
         (coords (mapcar 'line-to-coords lines)))
    (should (= (detect-overlapping-vents coords) 5))))

(ert-deftest 05-detect-overlapping-vents-input-data ()
  (let* ((lines (read-lines "./05.input.txt"))
         (coords (mapcar 'line-to-coords lines)))
    (should (= (detect-overlapping-vents coords) 6283))))

(ert-deftest 05-detect-overlapping-vents-diagonal-test-data ()
  (let* ((lines (read-lines "./05.test.txt"))
         (coords (mapcar 'line-to-coords lines)))
    (should (= (detect-overlapping-vents coords :diagonal t) 12))))

(ert-deftest 05-detect-overlapping-vents-diagonal-input-data ()
  (let* ((lines (read-lines "./05.input.txt"))
         (coords (mapcar 'line-to-coords lines)))
    (should (= (detect-overlapping-vents coords :diagonal t) 18864))))

(ert "05")
