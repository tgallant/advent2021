; https://adventofcode.com/2021/day/8

(require 'ert)

(defun unique-segment-lengths ()
  '(2 4 3 7))

(defun count-unique-segments (acc cur)
  (if (cl-find (length cur) (unique-segment-lengths))
      (+ acc 1)
    acc))

(defun count-unique-output (acc cur)
  (+ acc (cl-reduce 'count-unique-segments (cdr cur) :initial-value 0)))

(defun count-digits-with-unique-segments (displays)
  (cl-reduce 'count-unique-output displays :initial-value 0))

(defun not-contained-in (x y)
  (let ((y-list (string-to-list y)))
    (defun not-contains (acc cur)
      (if (seq-contains-p y-list cur)
          acc
        (cons cur acc)))
    (cl-reduce 'not-contains (string-to-list x) :initial-value nil)))

(defun make-signal-hash (signal)
  (cl-reduce '+ (string-to-list signal)))

(defun identify-0 (five signals)
  (if (and
       (= (length (not-contained-in (car signals) five)) 2)
       (= (length (not-contained-in five (car signals))) 1))
      (car signals)
    (identify-0 five (cdr signals))))

(defun identify-1 (signals)
  (if (= (length (car signals)) 2)
      (car signals)
    (identify-1 (cdr signals))))

(defun identify-2 (nine signals)
  (if (and
       (= (length (not-contained-in (car signals) nine)) 1)
       (= (length (not-contained-in nine (car signals))) 2))
      (car signals)
    (identify-2 nine (cdr signals))))

(defun identify-3 (seven signals)
  (if (and
       (= (length (not-contained-in (car signals) seven)) 2)
       (= (length (not-contained-in seven (car signals))) 0))
      (car signals)
    (identify-3 seven (cdr signals))))

(defun identify-4 (signals)
  (if (= (length (car signals)) 4)
      (car signals)
    (identify-4 (cdr signals))))

(defun identify-5 (six one signals)
  (if (and
       (= (length (not-contained-in (car signals) six)) 0)
       (= (length (not-contained-in one (car signals))) 1)
       (= (length (not-contained-in six (car signals))) 1))
      (car signals)
    (identify-5 six one (cdr signals))))

(defun identify-6 (one signals)
  (if (and
       (= (length (not-contained-in (car signals) one)) 5)
       (= (length (not-contained-in one (car signals))) 1))
      (car signals)
    (identify-6 one (cdr signals))))

(defun identify-7 (signals)
  (if (= (length (car signals)) 3)
      (car signals)
    (identify-7 (cdr signals))))

(defun identify-8 (signals)
  (if (= (length (car signals)) 7)
      (car signals)
    (identify-8 (cdr signals))))

(defun identify-9 (three signals)
  (if (and
       (= (length (not-contained-in (car signals) three)) 1)
       (= (length (not-contained-in three (car signals))) 0))
      (car signals)
    (identify-9 three (cdr signals))))

(defun make-digit-mapping (signals)
  (let* ((mapping (make-hash-table))
         (one (identify-1 signals))
         (four (identify-4 signals))
         (seven (identify-7 signals))
         (eight (identify-8 signals))
         (six (identify-6 one signals))
         (three (identify-3 seven signals))
         (five (identify-5 six one signals))
         (zero (identify-0 five signals))
         (nine (identify-9 three signals))
         (two (identify-2 nine signals))
         )
    (puthash (make-signal-hash one) "1" mapping)
    (puthash (make-signal-hash four) "4" mapping)
    (puthash (make-signal-hash seven) "7" mapping)
    (puthash (make-signal-hash eight) "8" mapping)
    (puthash (make-signal-hash six) "6" mapping)
    (puthash (make-signal-hash three) "3" mapping)
    (puthash (make-signal-hash nine) "9" mapping)
    (puthash (make-signal-hash two) "2" mapping)
    (puthash (make-signal-hash five) "5" mapping)
    (puthash (make-signal-hash zero) "0" mapping)
    (defun print-key (k v)
      (print (format "%s %d" v k)))
    (maphash 'print-key mapping)
    mapping))

(defun sum-mapped-display-value (acc cur)
  (let ((mapping (car acc))
        (sum (cdr acc))
        (hash (make-signal-hash cur)))
    (+ sum (gethash mapping hash))))

(defun generate-digit-output (display)
  (let ((mapping (make-digit-mapping (car display)))
        (output (cdr display)))
    (defun get-digit-from-mapping (val)
      (gethash (make-signal-hash val) mapping))
    (print (string-to-number (string-join (mapcar 'get-digit-from-mapping output))))
    (string-to-number (string-join (mapcar 'get-digit-from-mapping output)))))

(defun sum-display-output (displays)
  (cl-reduce '+ (mapcar 'generate-digit-output displays)))

(defun line-to-display (line)
  (let* ((parts (split-string line "|" t))
         (signals (split-string (nth 0 parts) " " t))
         (output (split-string (nth 1 parts) " " t)))
    (cons signals output)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 08-count-digits-with-unique-segments-test-data ()
  (let* ((lines (read-lines "./08.test.txt"))
         (displays (mapcar 'line-to-display lines)))
    (should (= (count-digits-with-unique-segments displays) 26))))

(ert-deftest 08-count-digits-with-unique-segments-input-data ()
  (let* ((lines (read-lines "./08.input.txt"))
         (displays (mapcar 'line-to-display lines)))
    (should (= (count-digits-with-unique-segments displays) 303))))

(ert-deftest 08-sum-display-output-test-data ()
  (let* ((lines (read-lines "./08.test.txt"))
         (displays (mapcar 'line-to-display lines)))
    (should (= (sum-display-output displays) 61229))))

(ert-deftest 08-sum-display-output-input-data ()
  (let* ((lines (read-lines "./08.input.txt"))
         (displays (mapcar 'line-to-display lines)))
    (should (= (sum-display-output displays) 61229))))

(ert "08")
