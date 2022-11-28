;; https://adventofcode.com/2021/day/3

(require 'ert)
(require 'seq)
(require 'subr-x)

(defun make-state (pos counts)
  `((pos . ,pos)
    (counts . ,counts)))

(defun count-occurrences (acc cur)
  (let* ((p (alist-get 'pos acc))
         (c (alist-get 'counts acc))
         (i (string-to-number cur))
         (v (plist-get c p))
         (n (plist-put v i (+ 1 (or (plist-get v i) 0)))))
    (make-state (+ 1 p) (plist-put c p n))))

(defun calculate (acc cur)
  (defun results ()
    (let ((chars (string-to-list cur))
          (state (make-state 0 acc)))
      (cl-reduce
       'count-occurrences chars
       :key 'char-to-string
       :initial-value state)))
  (alist-get 'counts (results)))

(defun plist-to-alist (pl)
  (defun build-alist (cur acc)
    (let* ((b (car acc))
           (r (cdr acc))
           (n `(,cur . ,b)))
      (if (eq b nil)
          `(,cur . ,r)
        `(nil . ,(cons n r)))))
  (cdr (cl-reduce 'build-alist pl :initial-value '() :from-end t)))

(defun most-common (counts)
  (let* ((c (cdr counts))
        (z (or (plist-get c 0) 0))
        (o (or (plist-get c 1) 0)))
    (if (> z o) "0" "1")))

(defun least-common (counts)
  (let* ((c (cdr counts))
        (z (or (plist-get c 0) 0))
        (o (or (plist-get c 1) 0)))
    (if (< z o) "0" "1")))

(defun compare-common-bits (fn vals)
  (string-to-number (string-join (mapcar fn vals)) 2))

(defun mcb (rates)
  (compare-common-bits 'most-common rates))

(defun lcb (rates)
  (compare-common-bits 'least-common rates))

(defun power-consumption (binary-list)
  (defun rates ()
    (cl-reduce 'calculate binary-list :initial-value '()))
  (let* ((r (plist-to-alist (rates)))
         (g (mcb r))
         (e (lcb r)))
    (* g e)))

(defun nth-of-string (str idx)
  (char-to-string (nth idx (string-to-list str))))

(defun nth-bits (lst n)
  (mapcar (lambda (s) (nth-of-string s n)) lst))

(defun mc (lst)
  (let ((count-0 (cl-count "0" lst :test 'equal))
        (count-1 (cl-count "1" lst :test 'equal)))
    (cond ((> count-0 count-1) "0")
          ((<= count-0 count-1) "1"))))

(defun lc (lst)
  (let ((count-0 (cl-count "0" lst :test 'equal))
        (count-1 (cl-count "1" lst :test 'equal)))
    (cond ((> count-0 count-1) "1")
          ((<= count-0 count-1) "0"))))

(defun calculate-ogr (acc cur)
  (let ((result (mc (nth-bits acc cur))))
    (defun starts-with-bit-p (s)
      (equal result (nth-of-string s cur)))
    (if (eq (length acc) 1) acc
      (seq-filter 'starts-with-bit-p acc))))

(defun calculate-csr (acc cur)
  (let ((result (lc (nth-bits acc cur))))
    (defun starts-with-bit-p (s)
      (equal result (nth-of-string s cur)))
    (if (eq (length acc) 1) acc
      (seq-filter 'starts-with-bit-p acc))))

(defun oxygen-generator-rating (binary-list)
  (let* ((max-idx (- (length (car binary-list)) 1))
         (range (number-sequence 0 max-idx)))
    (string-to-number
     (car (cl-reduce 'calculate-ogr range :initial-value binary-list))
     2)))

(defun c02-scrubber-rating (binary-list)
  (let* ((max-idx (- (length (car binary-list)) 1))
         (range (number-sequence 0 max-idx)))
    (string-to-number
     (car (cl-reduce 'calculate-csr range :initial-value binary-list))
     2)))

(defun life-support-rating (binary-list)
  (let ((o (oxygen-generator-rating binary-list))
        (c (c02-scrubber-rating binary-list)))
    (* o c)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 03-power-consumption-test-data ()
  (let* ((lines (read-lines "./03.test.txt")))
    (should (= (power-consumption lines) 198))))

(ert-deftest 03-power-consumption-input-data ()
  (let* ((lines (read-lines "./03.input.txt")))
    (should (= (power-consumption lines) 4006064))))

(ert-deftest 03-life-support-rating-test-data ()
  (let* ((lines (read-lines "./03.test.txt")))
    (should (= (life-support-rating lines) 230))))

(ert-deftest 03-life-support-rating-input-data ()
  (let* ((lines (read-lines "./03.input.txt")))
    (should (= (life-support-rating lines) 5941884))))

(ert "03")
