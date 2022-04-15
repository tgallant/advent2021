;; https://adventofcode.com/2021/day/15

(load (concat default-directory "heap.el"))

(defun get-val (x y points)
  (cond ((or (< x 0) (< y 0))
         nil)
        (t (nth x (nth y points)))))

(defun make-distances (start)
  (let ((distances (make-hash-table :test 'equal)))
    (puthash start 0 distances)
    distances))

(defun node-sort (a b)
  (< (cdr a) (cdr b)))

(defun make-node-queue (nodes start)
  (let ((queue (make-heap 'node-sort (length nodes))))
    (defun add-node (node)
      (if (equal node start)
          (heap-add queue (cons node 0))
        (heap-add queue (cons node 1.0e+INF))))
    (mapc 'add-node nodes)
    queue))

(defun distance-2d (a b)
  (let ((diffx (- (car a) (car b)))
        (diffy (- (cdr a) (cdr b))))
    (sqrt (+ (* diffx diffx) (* diffy diffy)))))

(defun find-shortest-path (graph start end)
  (cl-loop with node and node-dist and visited = (make-hash-table :test 'equal)
           and distances = (make-distances start)
           and nodes = (make-node-queue (hash-table-keys graph) start) while nodes
           do (setf node (car (heap-delete-root nodes)))
           if (equal node end) return distances
           do (puthash node t visited)
           do (setf node-dist (gethash node distances 1.0e+INF))
           do (cl-loop with d and p and alt for n in (gethash node graph)
                       do (setf d (gethash (car n) distances 1.0e+INF))
                       do (setf p (* 2 (distance-2d (car n) end)))
                       do (setf alt (+ node-dist (cdr n)))
                       if (and (not (gethash (car n) visited))
                               (< alt d))
                       do (progn
                            (heap-add nodes (cons (car n) (+ alt p)))
                            (puthash (car n) alt distances)))))

(defun add-edges (x y points graph)
  (defun add-edge (acc cur)
    (let* ((tx (car cur))
           (ty (cdr cur))
           (val (get-val tx ty points))
           (edge (cons cur val)))
      (if (eq val nil) acc
        (cons edge acc))))
  (let* ((node (cons x y))
         (up (cons x (- y 1)))
         (down (cons x (+ y 1)))
         (left (cons (- x 1) y))
         (right (cons (+ x 1) y))
         (neighbors (list up down left right))
         (edges (cl-reduce 'add-edge neighbors :initial-value '())))
    (puthash node edges graph)))

(defun make-graph (points)
  (cl-loop with graph = (make-hash-table :test 'equal)
           for y from 0 to (- (length points) 1)
           do (cl-loop for x from 0 to (- (length (nth 0 points)) 1)
                    do (add-edges x y points graph))
           finally return graph))

(defun line-to-list (line)
  (mapcar 'string-to-number (split-string line "" t)))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun test-find-shortest-path (file start)
  (let* ((lines (read-lines file))
         (leny (- (length lines) 1))
         (lenx (- (length (nth 0 lines)) 1))
         (end (cons lenx leny))
         (graph (make-graph (mapcar 'line-to-list lines)))
         (distances (find-shortest-path graph start end)))
    (print (length (hash-table-keys distances)))
    (gethash end distances)))

(defun profile-find-shortest-path (file)
  (profiler-start 'cpu)
  (test-find-shortest-path file '(0 . 0))
  (profiler-stop)
  (profiler-report))

(ert-deftest 15-find-shortest-path-test-data ()
  (should (= (test-find-shortest-path "./15.test.txt" '(0 . 0)) 40)))

(ert-deftest 15-find-shortest-path-input-data ()
  (should (= (test-find-shortest-path "./15.input.txt" '(0 . 0)) 390)))

(ert "15")
