; https://adventofcode.com/2021/day/4

(require 'ert)

(defun make-square (value checked?)
  `(,value . ,checked?))

(defun make-square-from-str (value)
  (make-square (string-to-number value) nil))

(defun make-row (row)
  (let ((parts (split-string row nil t)))
    (mapcar 'make-square-from-str parts)))

(defun make-card (numbers)
  (mapcar 'make-row numbers))

(defun sum-unchecked-row (row)
  (loop for square in row
        if (not (cdr square)) sum (car square)))

(defun sum-unchecked (card)
  (loop for row in card sum (sum-unchecked-row row)))

(defun calculate-score (pick card)
  (if card
      (* pick (sum-unchecked card))))

(defun apply-pick (pick cards)
  (defun apply-to-square (square)
    (if (= pick (car square))
        (make-square (car square) t)
      square))
  (defun apply-to-row (row)
    (mapcar 'apply-to-square row))
  (defun apply-to-card (card)
    (mapcar 'apply-to-row card))
  (mapcar 'apply-to-card cards))

(defun check-row (row)
  (loop for square in row
        with checked = t
        do (setq checked (and checked (cdr square)))
        finally return checked))

(defun card-col (card idx)
  (defun get-nth (cur)
    (nth idx cur))
  (mapcar 'get-nth card))

(defun check-card-rows (card)
  (loop for row in card
        if (check-row row) return t))

(defun check-card-cols (card)
  (loop for x from 0 to 4
        with col
        do (setq col (card-col card x))
        if (check-row col) return t))

(defun is-winning-card (card)
  (if (check-card-rows card) card
    (if (check-card-cols card) card)))

(defun find-winning-card (cards)
  (loop for card in cards
        if (is-winning-card card) return card))

(defun filter-winning-cards (cards)
  (defun filter-winning-card (acc cur)
    (if (is-winning-card cur)
        acc
      (cons cur acc)))
  (reduce 'filter-winning-card cards :initial-value '()))

(defun play-bingo (picks cards)
  (loop for pick in picks
        with c = cards
        with winning-card
        do (setq c (apply-pick pick c))
        do (setq winning-card (find-winning-card c))
        if winning-card return (calculate-score pick winning-card)))

(defun play-bingo-v2 (picks cards)
  (loop for pick in picks
        with c = cards
        with winning-score
        do (setq c (apply-pick pick c))
        do (setq winning-score (calculate-score pick (find-winning-card c)))
        do (setq c (filter-winning-cards c))
        if (= 0 (length c)) return winning-score))

(defun build-cards (acc cur)
  (let* ((cards (car acc))
         (buffr (cdr acc))
         (nextb (cons cur buffr)))
    (cond ((= (length nextb) 5)
           `(,(cons (make-card nextb) cards) . nil))
          ((< (length nextb) 5)
           `(,cards . ,(cons cur buffr))))))

(defun lines-to-cards (lines)
  (car (reduce 'build-cards lines :initial-value '())))

(defun line-to-picks (line)
  (let ((parts (split-string line "," t)))
    (mapcar 'string-to-number parts)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 04-play-bingo-test-data ()
  (let* ((lines (read-lines "./04.test.txt"))
         (picks (line-to-picks (car lines)))
         (cards (lines-to-cards (cdr lines))))
    (should (= (play-bingo picks cards) 4512))))

(ert-deftest 04-play-bingo-input-data ()
  (let* ((lines (read-lines "./04.input.txt"))
         (picks (line-to-picks (car lines)))
         (cards (lines-to-cards (cdr lines))))
    (should (= (play-bingo picks cards) 8580))))

(ert-deftest 04-play-bingo-v2-test-data ()
  (let* ((lines (read-lines "./04.test.txt"))
         (picks (line-to-picks (car lines)))
         (cards (lines-to-cards (cdr lines))))
    (should (= (play-bingo-v2 picks cards) 1924))))

(ert-deftest 04-play-bingo-v2-input-data ()
  (let* ((lines (read-lines "./04.input.txt"))
         (picks (line-to-picks (car lines)))
         (cards (lines-to-cards (cdr lines))))
    (should (= (play-bingo-v2 picks cards) 9576))))

(ert "04")
