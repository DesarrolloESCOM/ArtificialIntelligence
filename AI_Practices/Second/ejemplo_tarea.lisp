(load "~/quicklisp/setup.lisp")
(defconstant *CL-MAZE-PATH* "/home/alberto/Downloads/cl-maze-ubuntu-package/cl-maze/")

(pushnew *CL-MAZE-PATH* asdf:*central-registry* :test #'equal)
(ql:quickload "cl-maze")

(defpackage :tarea
   (:use :cl :cl-user :maze)
   (:export 
      :show-maze-interface
      :depth-first
      :breadth-first      
      :best-first
      :a-star))
(in-package :tarea)

(set-search-function 'depth-first "Búsqueda a lo profundo")
(set-search-function 'breadth-first "Búsqueda a lo ancho")
(set-search-function 'best-first "Búsqueda best-first")
(set-search-function 'a-star "Búsqueda A*")

(defparameter *OPENED* '())
(defparameter *CLOSED* '())
(defparameter *ID* 0)
(defconstant *OP* '("arriba"
                    "abajo"
                    "derecha"
                    "izquierda"
                    "arriba-derecha"
                    "arriba-izquierda"
                    "abajo-derecha"
                    "abajo-izquierda"))

(defun creaNodo(estado id-ancestro operador)
  (list (setq *ID* (+ 1 *ID*)) estado id-ancestro operador))

(defun insertaNodo(nodo)
  (setq *OPENED* (cons nodo *OPENED*)))

(defun extraeNodo(método)
  (cond ((equal 0 método) ;depth search
         (let ((n (first *OPENED*)))
         (setq *OPENED* (rest *OPENED*))
         (setq *CLOSED* (cons n *CLOSED*))
         n ))
        ((equal 1 método) 
         (let ((n (first (last *OPENED*)))) ;breadth search
         (setq *OPENED* (butlast *OPENED*))
         (setq *CLOSED* (cons n *CLOSED*))
         n ))) )

(defun rastreaSolución(nodo lista)
  (cond ((equal 1 (length lista)) lista)
        ((equal (third nodo) (first(second lista)))
            (cons nodo (rastreaSolución (second lista) (rest lista))))
        (T (rastreaSolución nodo (rest lista)))))

(defun operadorValido?(estado operador)
  (cond ((string= "arriba" operador)
         (cond ((null(member 1 (get-maze (first estado)(second estado)))) T)
               (T Nil)))
        ((string= "abajo" operador)
         (cond ((null(member 3 (get-maze (first estado)(second estado)))) T)
               (T Nil)))
        ((string= "derecha" operador)
         (cond ((null(member 2 (get-maze (first estado)(second estado)))) T)
               (T Nil)))
        ((string= "izquierda" operador)
         (cond ((null(member 4 (get-maze (first estado)(second estado)))) T)
               (T Nil)))
        ((string= "arriba-derecha" operador)
         (cond ((and (>= (- (first estado) 1) 0) (< (+ (second estado) 1)(get-maze-cols)))
                (cond ((and (or (null(member 2 (get-maze (first estado)(second estado))))
                                (null(member 2 (get-maze (- (first estado) 1)(second estado)))))
                           (or (null(member 2 (get-maze (- (first estado) 1)(second estado))))
                                (null(member 1 (get-maze (first estado)(+ (second estado) 1)))))
                           (or (null(member 1 (get-maze (first estado)(second estado))))
                                (null(member 2 (get-maze (first estado)(second estado)))))
                           (or (null(member 1 (get-maze (first estado)(second estado))))
                                (null(member 1 (get-maze (first estado)(+ (second estado) 1)))))) T)
                      (T Nil)))
               (T Nil)))
        ((string= "arriba-izquierda" operador)
         (cond ((and (>= (- (first estado) 1) 0) (>= (- (second estado) 1) 0))
                (cond ((and (or (null(member 4 (get-maze (first estado)(second estado))))
                                (null(member 4 (get-maze (- (first estado) 1)(second estado)))))
                           (or (null(member 4 (get-maze (- (first estado) 1)(second estado))))
                                (null(member 1 (get-maze (first estado)(- (second estado) 1)))))
                           (or (null(member 1 (get-maze (first estado)(second estado))))
                                (null(member 4 (get-maze (first estado)(second estado)))))
                           (or (null(member 1 (get-maze (first estado)(second estado))))
                                (null(member 1 (get-maze (first estado)(- (second estado) 1)))))) T)
                      (T Nil)))
               (T Nil)))
        ((string= "abajo-derecha" operador)
         (cond ((and (< (+ (first estado) 1)(get-maze-rows)) (< (+ (second estado) 1)(get-maze-cols)))
                (cond ((and (or (null(member 2 (get-maze (first estado)(second estado))))
                                (null(member 2 (get-maze (+ (first estado) 1)(second estado)))))
                           (or (null(member 2 (get-maze (+ (first estado) 1)(second estado))))
                                (null(member 3 (get-maze (first estado)(+ (second estado) 1)))))
                           (or (null(member 2 (get-maze (first estado)(second estado))))
                                (null(member 3 (get-maze (first estado)(second estado)))))
                           (or (null(member 3 (get-maze (first estado)(second estado))))
                                (null(member 3 (get-maze (first estado)(+ (second estado) 1)))))) T)
                      (T Nil)))
               (T Nil)))
        ((string= "abajo-izquierda" operador)
         (cond ((and (< (+ (first estado) 1)(get-maze-rows)) (>= (- (second estado) 1) 0))
                (cond ((and (or (null(member 4 (get-maze (first estado)(second estado))))
                                (null(member 4 (get-maze (+ (first estado) 1)(second estado)))))
                           (or (null(member 4 (get-maze (+ (first estado) 1)(second estado))))
                                (null(member 3 (get-maze (first estado)(- (second estado) 1)))))
                           (or (null(member 3 (get-maze (first estado)(second estado))))
                                (null(member 4 (get-maze (first estado)(second estado)))))
                           (or (null(member 3 (get-maze (first estado)(second estado))))
                                (null(member 3 (get-maze (first estado)(- (second estado) 1)))))) T)
                      (T Nil)))
               (T Nil)))))

(defun estadoValido?(estado)
  (cond ((and (and (>= (first estado) 0)(< (first estado)(get-maze-rows))) 
              (and (>= (second estado) 0)(< (second estado)(get-maze-cols)))) T)
        (T Nil)))

(defun nuevoEstado(estado operador)
  (cond ((string= "arriba" operador)(list (- (first estado) 1)(second estado)))
        ((string= "abajo" operador)(list (+ (first estado) 1)(second estado)))
        ((string= "derecha" operador)(list (first estado)(+ (second estado) 1)))
        ((string= "izquierda" operador)(list (first estado)(- (second estado) 1)))
        ((string= "arriba-derecha" operador)(list (- (first estado) 1)(+ (second estado) 1)))
        ((string= "arriba-izquierda" operador)(list (- (first estado) 1)(- (second estado) 1)))
        ((string= "abajo-derecha" operador)(list (+ (first estado) 1)(+ (second estado) 1)))
        ((string= "abajo-izquierda" operador)(list (+ (first estado) 1)(- (second estado) 1)))))

(defun recuerdo?(estado lista)
  (cond ((null lista) T)
        ((equal estado (second(first lista))) Nil)
        (T (recuerdo? estado (rest lista)))))

(defun expande(id estado método)
  (let ((operadores *OP*))
    (cond ((equal 0 método)(setq operadores (reverse operadores))))
    (dolist (n operadores)
      (cond ((and (operadorValido? estado n)(estadoValido?(nuevoEstado estado n))
                  (recuerdo?(nuevoEstado estado n) *CLOSED*)(recuerdo?(nuevoEstado estado n) *OPENED*))
             (insertaNodo(creaNodo (nuevoEstado estado n) id n) ))))))

(defun maxFrontera(longA longB)
  (cond ((> longA longB) longA)
        (T longB)))

;Depth-first

(defun depth-first (estado-inicial estado-meta)
  (setq *OPENED* '())
  (setq *CLOSED* '())
  (setq *ID* 0)

  (let ((nodo Nil)(sol '())(longitud 0)(time1 0)(time2 0))
    
    (setq time1 (get-internal-run-time))
    (insertaNodo (creaNodo estado-inicial -1 Nil) )
    (loop until (null *OPENED*) do
      (setq nodo (extraeNodo 0))
      (setq longitud (maxFrontera longitud (length *OPENED*)))
      (cond ((equal estado-meta (second nodo))
             (return))
            (T (expande (first nodo)(second nodo) 0))))
    (setq time2 (get-internal-run-time))

    (set-result-info :algorithm "Depth-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length (rastreaSolución nodo *CLOSED*))
                     :created-nodes (+ (length *CLOSED*)(length *OPENED*))
                     :explored-nodes (length *CLOSED*)
                     :max longitud)

    (dolist (y (butlast (rastreaSolución nodo *CLOSED*)))
      (push (fourth y) sol))
    sol))

;Breadth-First Search

(defun breadth-first (estado-inicial estado-meta)
  (setq *OPENED* '())
  (setq *CLOSED* '())
  (setq *ID* 0)

  (let ((nodo Nil)(sol '())(longitud 0)(time1 0)(time2 0))
    
    (setq time1 (get-internal-run-time))
    (insertaNodo (creaNodo estado-inicial -1 Nil) )
    (loop until (null *OPENED*) do
      (setq nodo (extraeNodo 1))
      (setq longitud (maxFrontera longitud (length *OPENED*)))
      (cond ((equal estado-meta (second nodo))
             (return))
            (T (expande (first nodo)(second nodo) 1))))
    (setq time2 (get-internal-run-time))

    (set-result-info :algorithm "Breadth-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length (rastreaSolución nodo *CLOSED*))
                     :created-nodes (+ (length *CLOSED*)(length *OPENED*))
                     :explored-nodes (length *CLOSED*)
                     :max longitud)

    (dolist (y (butlast (rastreaSolución nodo *CLOSED*)))
      (push (fourth y) sol))
    sol))

(defun aptitud(estado-actual estado-meta)
  (+ (abs (- (first estado-actual)(first estado-meta)))
     (abs (- (second estado-actual)(second estado-meta)))))

(defun creaNodo-Best(estado id-ancestro operador aptitud)
  (list (setq *ID* (+ 1 *ID*)) estado id-ancestro operador aptitud))

(defun expande-best(id estado estado-meta)
  (dolist (n *op*)
    (cond ((and (operadorValido? estado n)(estadoValido?(nuevoEstado estado n))
                (recuerdo?(nuevoEstado estado n) *CLOSED*)(recuerdo?(nuevoEstado estado n) *OPENED*))
           (insertaNodo(creaNodo-Best (nuevoEstado estado n) id n
                                      (aptitud (nuevoEstado estado n) estado-meta) ))))))

; best-first

(defun best-first (estado-inicial estado-meta)
  (setq *OPENED* '())
  (setq *CLOSED* '())
  (setq *ID* 0)

  (let ((nodo Nil)(sol '())(longitud 0)(time1 0)(time2 0))
    
    (setq time1 (get-internal-run-time))
    (insertaNodo (creaNodo-Best estado-inicial -1 Nil (aptitud estado-inicial estado-meta) ))
    (loop until (null *OPENED*) do
      (setq nodo (extraeNodo 0))
      (setq longitud (maxFrontera longitud (length *OPENED*)))
      (cond ((equal estado-meta (second nodo))
             (return))
            (T (expande-best (first nodo)(second nodo) estado-meta)))
      (setq *OPENED* (sort *OPENED* #'< :key #'fifth)))
    (setq time2 (get-internal-run-time))

    (set-result-info :algorithm "Best-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length (rastreaSolución nodo *CLOSED*))
                     :created-nodes (+ (length *CLOSED*)(length *OPENED*))
                     :explored-nodes (length *CLOSED*)
                     :max longitud)

    (dolist (y (butlast (rastreaSolución nodo *CLOSED*)))
      (push (fourth y) sol))
    sol))

(defun creaNodo-a-star(estado id-ancestro operador aptitud costo)
  (list (setq *ID* (+ 1 *ID*)) estado id-ancestro operador aptitud costo (+ aptitud costo)))

(defun expande-a-star(id estado estado-meta costo)
  (dolist (n *op*)
    (cond ((and (operadorValido? estado n)(estadoValido?(nuevoEstado estado n))
                (recuerdo?(nuevoEstado estado n) *CLOSED*))
           (cond ((not(recuerdo?(nuevoEstado estado n) *OPENED*))
                  (cond ((< (seventh (nth (recuerdoIndex (nuevoEstado estado n) *OPENED* 0) *OPENED*))
                            (+ (aptitud (nuevoEstado estado n) estado-meta)(+ costo 1)))
                         (setf (nth (recuerdoIndex (nuevoEstado estado n) *OPENED* 0) *OPENED*)
                               (creaNodo-a-star (nuevoEstado estado n) id n
                                                (aptitud (nuevoEstado estado n) estado-meta)
                                                (+ costo 1))))))
                 (T (insertaNodo(creaNodo-a-star (nuevoEstado estado n) id n
                                                 (aptitud (nuevoEstado estado n) estado-meta)
                                                 (+ costo 1)))))))))

(defun recuerdoIndex(estado lista index)
  (cond ((null lista) Nil)
        ((equal estado (second(first lista))) index)
        (T (recuerdoIndex estado (rest lista) (+ index 1)))))

; a-star

(defun a-star (estado-inicial estado-meta)
  (setq *OPENED* '())
  (setq *CLOSED* '())
  (setq *ID* 0)

  (let ((nodo Nil)(sol '())(longitud 0)(time1 0)(time2 0))
    
    (setq time1 (get-internal-run-time))
    (insertaNodo (creaNodo-a-star estado-inicial -1 Nil (aptitud estado-inicial estado-meta) 0))
    (loop until (null *OPENED*) do
      (setq nodo (extraeNodo 0))
      (setq longitud (maxFrontera longitud (length *OPENED*)))
      (cond ((equal estado-meta (second nodo))
             (return))
            (T (expande-a-star (first nodo)(second nodo) estado-meta (sixth nodo))))
      (setq *OPENED* (sort *OPENED* #'< :key #'seventh)))
    (setq time2 (get-internal-run-time))

    (set-result-info :algorithm "A* Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length (rastreaSolución nodo *CLOSED*))
                     :created-nodes (+ (length *CLOSED*)(length *OPENED*))
                     :explored-nodes (length *CLOSED*)
                     :max longitud)

    (dolist (y (butlast (rastreaSolución nodo *CLOSED*)))
      (push (fourth y) sol))
    sol))