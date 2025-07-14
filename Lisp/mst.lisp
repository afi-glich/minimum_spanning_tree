; Mode: Lisp

#|Afifa Saeed, 835367 
ha collaborato con Jhordan Steve Rodriguez Rojas, 832966
e con Sharmin Anthuane Camacho Rojas, 793661
|# 

; MST In Lisp


(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *vertex-key* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))


(defun is-graph (graph-id)
  (cond ((null graph-id)
         (error "Graph-id non valid."))
        (T (gethash graph-id *graphs*))))


(defun new-graph (graph-id)
  (cond ((null graph-id)
         (error "Graph-id non valid."))
        (T
         (or (gethash graph-id *graphs*)
             (setf (gethash graph-id *graphs*) graph-id)))))


(defun delete-graph (graph-id)
  (cond ((null (is-graph graph-id))
         (error "Graph does not exist"))
        (T
         (remhash graph-id *graphs*)
         (maphash #'(lambda (k v) (declare (ignore v))
                      (cond ((eql (second k) graph-id)
                             (remhash k *arcs*)))) 
                  *arcs*)
         (maphash #'(lambda (k v) (declare (ignore v))
                      (cond ((eql (second k) graph-id)
                             (remhash k *vertices*))))
                  *vertices*)
         (maphash #'(lambda (k v) (declare (ignore v))
                      (cond ((eql (second k) graph-id)
                             (remhash k *vertex-key*))))
                  *vertex-key*)
         (maphash #'(lambda (k v) (declare (ignore v))
                      (cond ((eql (second k) graph-id)
                             (remhash k *previous*))))
                  *previous*)
         (maphash #'(lambda (k v) (declare (ignore v))
                      (cond ((eql (second k) graph-id)
                             (remhash k *visited*))))
                  *visited*)))) 


(defun new-vertex (graph-id vertex-id)
  (cond ((not (or (symbolp vertex-id)
                  (numberp vertex-id)))
         (error "Vertex-id must be a symbol or a number"))
        ((is-graph graph-id)
         (or (gethash (list 'vertex graph-id vertex-id) *vertices*) 
             (setf (gethash (list 'vertex graph-id vertex-id)
                            *vertices*)
                   (list 'vertex graph-id vertex-id))))))


(defun graph-vertices (graph-id)
  (cond ((is-graph graph-id)
         (let ((vertices-list '()))
           (maphash #'(lambda (k v)
                        (cond ((eql (second k) graph-id)
                               (push v vertices-list))))  
                    *vertices*)
           vertices-list))
        (T (error "Graph not found."))))



(defun is-vertex (graph-id vertex-id)
  (cond ((not (or (symbolp vertex-id)
                  (numberp vertex-id)))
         (error "Vertex-id non valid."))
        ((null (gethash (list 'vertex graph-id vertex-id) *vertices*))
         NIL)
        (T T)))


(defun new-arc (graph-id vertex-id1 vertex-id2 &optional (weight 1))
  (cond ((> weight 0)
         (new-graph graph-id)
         (new-vertex graph-id vertex-id1)
         (new-vertex graph-id vertex-id2)
         (let ((arc (or 
                     (gethash (list 'arc graph-id vertex-id1
                                    vertex-id2) *arcs*)
                     (gethash (list 'arc graph-id vertex-id2
                                    vertex-id1) *arcs*))))
           (cond ((not (null arc))
                  (remhash (list 'arc graph-id vertex-id1 vertex-id2)
                           *arcs*) 
                  (remhash (list 'arc graph-id vertex-id2 vertex-id1) 
                           *arcs*)))) 
         (setf (gethash (list 'arc graph-id vertex-id1 vertex-id2) 
                        *arcs*) 
               (list 'arc graph-id vertex-id1 vertex-id2 weight)))))


(defun graph-arcs (graph-id)
  (cond ((is-graph graph-id)
         (let ((arc-rep-list '()))
           (maphash #'(lambda (key value)
                        (cond ((eql graph-id (second key)) 
                               (push value arc-rep-list)))) *arcs*)
           arc-rep-list))
        (T (error "Graph not found."))))


(defun graph-vertex-neighbors (graph-id vertex-id)
  (cond ((and (is-graph graph-id)
              (is-vertex graph-id vertex-id))
         (let ((arc-rep-list '()))
           (maphash #'(lambda (k v)
                        (cond ((and (eql graph-id (second k)) 
                                    (eql vertex-id (third k))) 
                               (push v arc-rep-list)) 
                              ((and (eql graph-id (second k))
                                    (eql vertex-id (fourth k))) 
                               (push (list 'arc graph-id vertex-id
                                           (third v) (fifth v))
                                     arc-rep-list))))
                    *arcs*)
           arc-rep-list))))


(defun graph-vertex-adjacent (graph-id vertex-id)
  (cond ((and (is-graph graph-id)
              (is-vertex graph-id vertex-id))
         (let ((vertex-rep-list '()))
           (maphash #'(lambda (k v)
                        (cond ((and (eql graph-id (second k))
                                    (eql vertex-id (third k)))
                               (push (list 'vertex graph-id 
                                           (fourth v)) 
                                     vertex-rep-list))
                              ((and (eql graph-id (second k))
                                    (eql vertex-id (fourth k)))
                               (push (list 'vertex graph-id 
                                           (third v))
                                     vertex-rep-list)))) 
                    *arcs*)
           vertex-rep-list))))


(defun graph-print (graph-id)
  (cond ((is-graph graph-id)
         (format t "Vertici di ~S: ~%" graph-id)
         (print-vertices (graph-vertices graph-id))
         (format t "Archi di ~S: ~%" graph-id)
         (print-arcs (graph-arcs graph-id)))
        (T (error "Graph not found."))))


(defun print-vertices (vertices)
  (cond ((not (null vertices))
         (format t "~S~%" (first vertices))
         (print-vertices (rest vertices)))))


(defun print-arcs (arcs)
  (cond ((not (null arcs))
         (format t "~S~%" (first arcs))
         (print-vertices (rest arcs)))))


(defun new-heap (heap-id &optional (capacity 42))
  (cond ((not (symbolp heap-id))
         (error "Heap-id not valid symbol"))
        (T 
         (or (gethash heap-id *heaps*)
             (setf (gethash heap-id *heaps*) 
                   (list 'heap heap-id 0 
                         (make-array
                          capacity
                          :adjustable t)))))))    


(defun heap-id (heap-rep)
  (cond ((null heap-rep)
         (error "Argument non valid."))
        (T (second heap-rep))))


(defun heap-size (heap-rep)
  (cond ((null heap-rep)
         (error "Argument non valid."))
        (T (third heap-rep))))


(defun heap-actual-heap (heap-rep)
  (cond ((null heap-rep)
         (error "Argument non valid.")) 
        (T (fourth heap-rep))))


(defun heap-delete (heap-id)
  (cond ((symbolp heap-id)
         (remhash heap-id *heaps*))
        (T (error "Heap-id non valid."))))


(defun heap-empty (heap-id)
  (cond ((null (gethash heap-id *heaps*))
         (error "Heap does not exist")))
  (zerop (heap-size (gethash heap-id *heaps*))))


(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))


(defun heap-head (heap-id)
  (cond ((heap-not-empty heap-id)
         (aref (heap-actual-heap (gethash heap-id *heaps*)) 1))))


(defun heap-insert (heap-id k v)
  (cond ((not (symbolp heap-id))
         (error "Heap-id non valid.")))
  (let ((heap-size (heap-size (gethash heap-id *heaps*)))
        (heap-array (heap-actual-heap (gethash heap-id *heaps*))))
     (remhash heap-id *heaps*)
     (cond ((= heap-size (- (length heap-array) 1))
            (setf (gethash heap-id *heaps*) 
                  (list 'heap heap-id
                        (+ heap-size 1)
                        (adjust-array heap-array (* heap-size 2)))))
           (T (setf (gethash heap-id *heaps*) 
                  (list 'heap heap-id
                        (+ heap-size 1)
                        heap-array))))
     (setf (aref heap-array (+ heap-size 1)) (list k v))
     (build-min-heap heap-array (+ heap-size 1))) T) 


(defun build-min-heap (h-array h-size)
  (cond ((and (> h-size 1)
              (< (first (aref h-array h-size)) 
                 (first (aref h-array (floor h-size 2)))))
         (let ((min (aref h-array h-size)))
           (setf (aref h-array h-size) 
                 (aref h-array (floor h-size 2)))
           (setf (aref h-array (floor h-size 2))
                 min)
           (build-min-heap h-array
                           (floor h-size 2))))))

                
(defun heap-extract (heap-id)
  (cond ((heap-not-empty heap-id) 
         (let* ((h-head (heap-head heap-id)) 
                (h-array (fourth (gethash heap-id *heaps*))) 
                (h-size (heap-size (gethash heap-id *heaps*)))) 
           (remhash heap-id *heaps*)
           (setf (gethash heap-id *heaps*)
          (list 'heap 
                heap-id 
                (- h-size 1)
                h-array))
           (setf (aref h-array 1) (aref h-array h-size)) 
           (setf (aref h-array h-size) 'nil)
           (rebuild-min-heap 
            (heap-actual-heap (gethash heap-id *heaps*)) 1) 
           h-head))))


(defun rebuild-min-heap (h-array pos)
  ;; non ha figli
  (cond ((or (> (* pos 2) (- (length h-array) 1))
             (> (+ (* pos 2) 1) (- (length h-array) 1)))
         T)
        ;; ha entrambi i figli
        ((and (not (null (aref h-array (* 2 pos))))
              (not (null (aref h-array (+ (* 2 pos) 1)))))
         (let* ((left-key (aref h-array (* 2 pos)))
                (right-key (aref h-array (+ (* 2 pos) 1)))
                (current-key (aref h-array pos)))
           (cond ((and (>= (first left-key)
                           (first right-key))
                       (< (first right-key)
                          (first current-key)))
                  (setf (aref h-array pos) right-key)
                  (setf (aref h-array (+ (* 2 pos) 1))
                        current-key) 
                  (rebuild-min-heap h-array (+ (* 2 pos) 1)))
                 ((and (< (first left-key)
                          (first right-key))
                       (< (first left-key)
                          (first current-key)))
                  (setf (aref h-array pos) left-key)
                  (setf (aref h-array (* 2 pos)) current-key)
                  (rebuild-min-heap h-array (* 2 pos))))))
        ;; ha un solo figlio (figlio sinitro)
        ((and (not (null (aref h-array (* 2 pos))))
              (null (aref h-array (+ (* 2 pos) 1))))
         (let ((left-key (aref h-array (* 2  pos)))
                (current-key (aref h-array pos)))
           (cond ((> (first current-key)
                     (first left-key))
                  (setf (aref h-array (* 2 pos)) current-key)
                  (setf (aref h-array pos) left-key)))))))


(defun heap-print (heap-id)
  (cond ((or (not (symbolp heap-id))
              (null (gethash heap-id *heaps*)))
         (error "No heap found.")))
  (let ((heap (gethash heap-id *heaps*)))
    (format t "Lo heap contiene ~S elementi ~%" (third heap))
    (print-array (fourth heap) 1 (third heap))))


(defun print-array (array position max-size)
  (cond ((<= position max-size)
         (format t "~S: ( ~S  ~S~ )%"
                 position
                 (first (aref array position))
                 (second (aref array position)))
         (print-array array (+ position 1) max-size))))
        

(defun mst-vertex-key (graph-id vertex-id)
  (cond ((and (is-graph graph-id)
              (is-vertex graph-id vertex-id))
         (gethash (list 'mst-vertex-key graph-id vertex-id)
                  *vertex-key*)))) 


(defun mst-previous (graph-id vertex-id)
  (cond ((and (is-graph graph-id)
              (is-vertex graph-id vertex-id))
         (gethash (list 'mst-previous graph-id vertex-id)
                  *previous*)))) 
         

(defun mst-prim (graph-id source)
  (cond ((null (is-graph graph-id))
         (error "Graph does not exist."))
        ((not (is-vertex graph-id source))
         (error "Vertex not found."))
        (T
         (new-heap graph-id)
         (cond ((not (null (mst-vertex-key graph-id source))) 
                (remove-graph-mst graph-id)))
         (initialize-vertices graph-id 
                              (remove (list 'vertex graph-id source) 
                                      (graph-vertices graph-id)
                                      :test #'equal))  
         (setf (gethash (list 'mst-vertex-key graph-id source)
                        *vertex-key*) 0) 
         (setf (gethash (list 'mst-previous graph-id source)
                        *previous*) 'nil) 
         (setf (gethash (list 'mst-visited graph-id source) 
                        *visited*) 1) 
         (set-heap graph-id (graph-vertex-neighbors graph-id source))
         (explore-graph graph-id))) NIL)


(defun remove-graph-mst (graph-id)
  (maphash #'(lambda (k v) (declare (ignore v))
               (cond ((eql (second k) graph-id)
                      (remhash k *vertex-key*)))) 
           *vertex-key*)
  (maphash #'(lambda (k v) (declare (ignore v))
               (cond ((eql (second k) 
                           graph-id) 
                      (remhash k *previous*)))) 
           *previous*)
  (maphash #'(lambda (k v) (declare (ignore v))
               (cond ((eql (second k) 
                           graph-id) 
                      (remhash k *visited*))))
           *visited*))



(defun initialize-vertices (graph-id vertices)
  (cond ((not (null vertices))
         (setf (gethash (list 'mst-vertex-key 
                              graph-id 
                              (first (third vertices)))
                        *vertex-key*) 
               most-positive-double-float) 
         (initialize-vertices graph-id (rest vertices)))))


;; estrae la radice, se vertice non è nel grafo, lo aggiunge

(defun explore-graph (graph-id)
  (cond ((heap-not-empty graph-id)
         (let ((u (heap-extract graph-id)))
           (cond ((null (gethash (list 'mst-visited graph-id 
                                       (fourth (second u)))
                                 *visited*))
                  (setf (gethash (list 'mst-vertex-key
                                       graph-id 
                                       (fourth (second u)))
                                 *vertex-key*) 
                        (first u))
                  (setf (gethash (list 'mst-previous 
                                       graph-id 
                                       (fourth (second u)))
                                 *previous*)
                        (third (second u)))
                  (setf (gethash (list 'mst-visited 
                                       graph-id 
                                       (fourth (second u)))
                                 *visited*)
                        1)
                  (set-heap graph-id (graph-vertex-neighbors 
                                      graph-id 
                                      (fourth (second u)))))))  
         (explore-graph graph-id))
        ((heap-empty graph-id)
         (heap-delete graph-id)))) 

         

(defun set-heap (graph-id arcs)
  (cond ((not (null arcs))
         (cond ((null (gethash (list graph-id (fourth (first arcs)))
                               *visited*)) 
                (heap-insert graph-id (fifth (first arcs)) 
                             (first arcs))))
         (set-heap graph-id (rest arcs)))))
        

(defun mst-get (graph-id source)
  (cond ((and (is-graph graph-id)
              (is-vertex graph-id source))
         (pre-order (list 'arc graph-id 'nil source 0)))))


;; estrae lista ordinata dei vertici raggiungibili da vertex e fa
;; visita preorder

(defun pre-order (arc)
  (let ((lista (sort (sort (get-vertex-list (second arc) (fourth arc))
                     #'compare-4 :key #'fourth) #'< :key #'fifth))) 
    (cond ((> (length lista) 0)
           (cond ((not (equal 'nil (third arc)))
                  (append (list arc)
                          (mapcan #'pre-order lista)))
                 (T (append '() (mapcan #'pre-order lista)))))
          (T (list arc))))) 


;; define a comparator for sort

(defun compare-4 (a b)
  (string< (write-to-string a)
           (write-to-string b)))


;; per ricavare la lista dei vertici direttamente raggiungibili dal
;; dal vertice s

(defun get-vertex-list (graph-id s)
  (let ((valori'()))
    (maphash #'(lambda (k v)
                 (cond ((and (eql graph-id (second k)) 
                             (eql v s))
                        (push 
                         (list 'arc graph-id v (third k)  
                               (or (fifth 
                                    (gethash 
                                     (list 'arc graph-id v (third k))
                                     *arcs*)) 
                                   (fifth 
                                    (gethash 
                                     (list 'arc graph-id (third k) v)
                                     *arcs*)))) 
                         valori)))) 
             *previous*)
    valori))
