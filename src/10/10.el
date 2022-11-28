;; https://adventofcode.com/2021/day/10

(require 'ert)

(defconst opening-characters '("(" "[" "{" "<"))

(defconst closing-characters '(")" "]" "}" ">"))

(defconst tag-pairs '(("(" . ")")
                      ("[" . "]")
                      ("{" . "}")
                      ("<" . ">")))

(defconst tag-score '((")" . 3)
                      ("]" . 57)
                      ("}" . 1197)
                      (">" . 25137)))

(defconst autocomplete-score '((")" . 1)
                               ("]" . 2)
                               ("}" . 3)
                               (">" . 4)))

(defun is-opening-character (char)
  (if (member char opening-characters) t))

(defun is-closinig-character (char)
  (if (member char closing-characters) t))

(defun get-closing-tag (tag)
  (alist-get tag tag-pairs nil nil 'string-equal))

(defun get-tag-score (tag)
  (alist-get tag tag-score 0 nil 'string-equal))

(defun get-autocomplete-score (tag)
  (alist-get tag autocomplete-score 0 nil 'string-equal))

(defun detect-syntax-error (line)
  (loop with close-stack for char in line
        if (is-opening-character char)
          do (push (get-closing-tag char) close-stack)
        if (is-closinig-character char)
          if (string-equal char (car close-stack))
            do (setq close-stack (cdr close-stack))
          else
            return char))

(defun calculate-syntax-error-score (lines)
  (let* ((errors (mapcar 'detect-syntax-error lines))
         (scores (mapcar 'get-tag-score errors)))
    (reduce '+ scores)))

(defun tag-autocomplete (line)
  (loop with close-stack for char in line
        if (is-opening-character char)
          do (push (get-closing-tag char) close-stack)
        if (is-closinig-character char)
          if (string-equal char (car close-stack))
            do (setq close-stack (cdr close-stack))
          else
            return nil
        finally return close-stack))

(defun score-autocompletion (tags)
  (defun score (acc cur)
    (+ (* acc 5) (get-autocomplete-score cur)))
  (reduce 'score tags :initial-value 0))

(defun remove-nils (lst)
  (defun inner (cur acc)
    (if (eq cur nil) acc
      (cons cur acc)))
  (reduce 'inner lst :initial-value '() :from-end t))

(defun calculate-autocomplete-score (lines)
  (let* ((autocompletions (mapcar 'tag-autocomplete lines))
         (filtered (remove-nils autocompletions))
         (scores (mapcar 'score-autocompletion filtered))
         (sorted (sort scores #'>)))
    (nth (/ (length sorted) 2) sorted)))

(defun line-to-list (line)
  (split-string line "" t))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-calculate-syntax-error-score (file)
  (let* ((lines (read-lines file))
         (split-lines (mapcar 'line-to-list lines)))
    (calculate-syntax-error-score split-lines)))

(defun test-calculate-autocomplete-score (file)
  (let* ((lines (read-lines file))
         (split-lines (mapcar 'line-to-list lines)))
    (calculate-autocomplete-score split-lines)))

(ert-deftest 10-calculate-syntax-error-score-test-data ()
  (should (= (test-calculate-syntax-error-score "./10.test.txt") 26397)))

(ert-deftest 10-calculate-syntax-error-input-data ()
  (should (= (test-calculate-syntax-error-score "./10.input.txt") 339477)))

(ert-deftest 10-calculate-autocomplete-score-test-data ()
  (should (= (test-calculate-autocomplete-score "./10.test.txt") 288957)))

(ert-deftest 10-calculate-autocomplete-score-input-data ()
  (should (= (test-calculate-autocomplete-score "./10.input.txt") 3049320156)))

(ert "10")
