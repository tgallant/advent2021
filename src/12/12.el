;; https://adventofcode.com/2021/day/12

(require 'ert)

(defconst start-node '(start . t))

(defconst end-node '(end . t))

(defun lower-p (s)
  (let ((c (string-to-char s)))
    (string-equal "Ll" (get-char-code-property c 'general-category))))

(defun string-to-node (s)
  (cons (intern s) (lower-p s)))

(defun line-to-edge (line)
  (mapcar 'string-to-node (split-string line "-" t)))

(defun add-edge (graph edge)
  (let ((n1 (nth 0 edge))
        (n2 (nth 1 edge)))
    (puthash n1 (cons n2 (gethash n1 graph)) graph)
    (puthash n2 (cons n1 (gethash n2 graph)) graph)
    graph))

(defun make-graph (edges)
  (cl-reduce 'add-edge edges :initial-value (make-hash-table :test 'equal)))

(defun find-paths (graph)
  (defun traverse (start)
    (let ((paths '()))
      (defun inner (node path)
        (cond ((equal node end-node)
               (push (cons node path) paths))
              ((cdr node)
               (if (not (member node path))
                   (mapc (lambda (n) (inner n (cons node path))) (gethash node graph))))
              (t (mapc (lambda (n) (inner n (cons node path))) (gethash node graph)))))
      (inner start (list start-node))
      paths))
  (let ((start-nodes (gethash start-node graph)))
    (apply 'append (mapcar 'traverse start-nodes))))

(defun find-paths-v2 (graph)
  (defun traverse (start)
    (let ((paths '()))
      (defun inner (node data)
        (let ((path (car data))
              (twice (cdr data)))
          (cond ((equal node end-node)
                 (push (cons node path) paths))
                ((cdr node)
                 (if (not (member node path))
                     (mapc (lambda (n) (inner n (cons (cons node path) twice))) (gethash node graph))
                   (if (and (= 1 (count node path :test 'equal))
                            (not twice)
                            (not (equal node start-node))
                            (not (equal node end-node)))
                       (mapc (lambda (n) (inner n (cons (cons node path) t))) (gethash node graph)))))
                (t (mapc (lambda (n) (inner n (cons (cons node path) twice))) (gethash node graph))))))
      (inner start (cons (list start-node) nil))
      paths))
  (let ((start-nodes (gethash start-node graph)))
    (apply 'append (mapcar 'traverse start-nodes))))

(defun count-paths (graph)
  (length (find-paths graph)))

(defun count-paths-v2 (graph)
  (length (find-paths-v2 graph)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-count-paths (file)
  (let* ((lines (read-lines file))
         (edges (mapcar 'line-to-edge lines))
         (graph (make-graph edges)))
    (count-paths graph)))

(defun test-count-paths-v2 (file)
  (let* ((lines (read-lines file))
         (edges (mapcar 'line-to-edge lines))
         (graph (make-graph edges)))
    (count-paths-v2 graph)))

(ert-deftest 12-count-paths-small-test-data ()
  (should (= (test-count-paths "./12.test.small.txt") 10)))

(ert-deftest 12-count-paths-med-test-data ()
  (should (= (test-count-paths "./12.test.med.txt") 19)))

(ert-deftest 12-count-paths-large-test-data ()
  (should (= (test-count-paths "./12.test.large.txt") 226)))

(ert-deftest 12-count-paths-input-data ()
  (should (= (test-count-paths "./12.input.txt") 4411)))

(ert-deftest 12-count-paths-v2-small-test-data ()
  (should (= (test-count-paths-v2 "./12.test.small.txt") 36)))

(ert-deftest 12-count-paths-v2-med-test-data ()
  (should (= (test-count-paths-v2 "./12.test.med.txt") 103)))

(ert-deftest 12-count-paths-v2-large-test-data ()
  (should (= (test-count-paths-v2 "./12.test.large.txt") 3509)))

(ert-deftest 12-count-paths-v2-input-data ()
  (should (= (test-count-paths-v2 "./12.input.txt") 136767)))

(ert "12-count-paths")
