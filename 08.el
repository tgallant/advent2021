; https://adventofcode.com/2021/day/8

(defun unique-segment-lengths ()
  '(2 4 3 7))

(defun count-unique-segments (acc cur)
  (if (find (length cur) (unique-segment-lengths))
      (+ acc 1)
    acc))

(defun count-unique-output (acc cur)
  (+ acc (reduce 'count-unique-segments (cdr cur) :initial-value 0)))

(defun count-digits-with-unique-segments (displays)
  (reduce 'count-unique-output displays :initial-value 0))

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

(ert "08")
