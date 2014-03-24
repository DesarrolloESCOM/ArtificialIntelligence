(defvar *contadorNodo* 0)
(defvar *closedList* nil)
(defvar *openList* nil)
(defvar *closedList* nil)
(defvar *EstadosNodos* '(SinVisitar Visitado EnEspera))
;; SE CAMBIO Norte = 1, Sur = 3, Este = 2, Oeste = 4
;; Se define la casilla de inicio y la casilla de fin
(defvar *Entrada* (list 0 0))
(defvar *Salida* (list 0 0))
;(defvar *Movimientos* '(Norte Sur Este Oeste Noreste Noroeste Sureste Suroeste))
(defvar *PosicionObstaculos* (list 1 3 2 4))
; para los Laberintos
(defvar *Laberinto* (make-array '(100 100) :element-type 'list :initial-element nil))
(defun read-datafile (path-to-file)
  (let ((renglones 0) (columnas 0))
    (with-open-file (stream path-to-file)
      (setq renglones (read stream nil nil))
      (setq columnas (read stream nil nil))
      ;;Se obtiene la información de la entrada del laberinto y su salida =========== OPCIONAL
      (setq *Entrada* (read stream nil nil))
      (setq *Salida* (read stream nil nil))
      (setf *Laberinto* (adjust-array *Laberinto* (list renglones columnas)))
      
      (read-line stream nil nil)
      
      (loop for renglon from 0 below renglones do
        (loop for columna from 0 below columnas do
          (setf (aref *Laberinto* renglon columna) (read stream nil nil))
          )
        )
      )
    )
  )
;------------------------------------------------------------------------------
;funciones de utileria general para el manejo de los arboles/grafos de busqueda
;------------------------------------------------------------------------------
;MACRO WHILE, norvig.com
(defmacro while (test &rest body)
  "Repeat body while test is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))
;Distancia Manhattan
(defun distanciaManhattan (PosicionA PosicionB)
  (cond
    ((equal (length PosicionA) (length PosicionB))
      (let ((distancia 0))
        (loop for i from 0 below (length PosicionB) do 
              (incf distancia (abs (- (nth i PosicionA) (nth i PosicionB))))
           )
        distancia
        )
     )
    (T
      (print (string "No se puede obtener la distancia manhattan, los vectores deben tener las mismas dimensiones"))
      0
      )
    )
  )
;---------------------------------------------------
(defstruct Nodo idNodo ancestroNodo costoNodo estadoNodo accionNodo posicionNodo)
(defun append-if (&key elemento lista)
  (cond
    ((not (numberp (position elemento lista)))
      (append (list elemento) lista)
      )
    )
  )
(defun prepend-if (&key elemento lista)
  (cond
    ((not (numberp (position elemento lista)))
      (append lista (list elemento))
      )
    )
  )
(defun crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)))
  ; El id se auto incrementa en 1 cada que se crea un nodo nuevo
  ; El ancestro hace referencia al id del nodo de donde se llego al nodo actual
  ; El costo estará dado por la heuristica (Best First Search, A*, para otros algoritmos)
  ; El estado será en espera, visitado, sin visitar
  ; La accion estará denotada por el paso que se tuvo que realizar para llegar al nodo
  ; La posicion del nodo estara dado en coordenadas (renglon columna) para poder ubicarlo en el laberinto
  (incf *contadorNodo* 1)
  (make-Nodo :idNodo *contadorNodo* :ancestroNodo ancestro :costoNodo costo :estadoNodo estado :accionNodo accion :posicionNodo posicion)
  )
(defun agregarNodoListaAbierta (nodo &optional modo)
  (cond
    ((equal modo 'stack)
      (setf *openList* (append-if :lista *openList* :elemento nodo )) 
    )
    (T
      (setf *openList* (prepend-if :lista *openList* :elemento nodo))
      )
   )
 )
(defun agregarNodoListaCerrada(nodo)
  (append (list nodo) *closedList*)
  (setf *openList* (delete-if #'(lambda (x) (equal x nodo)) *openList*))
  )
(defun valoraCosto (NodoPadre CasillaObjetivo &optional (algoritmo nil))
  (cond
    ((null algoritmo)
     (+ (Nodo-costoNodo NodoPadre) 1) ;No hay algoritmo a evaluar
     )
    ((equal algoritmo 'A*)
     ;Se calcula utilizando f(x) = g(x) + H(x)
     (+ (Nodo-costoNodo NodoPadre) (distanciaManhattan *Salida* CasillaObjetivo) )
     )
    ((equal algoritmo 'BFS)
     ;Solo se calcula la distancia manhattan
     (distanciaManhattan *Salida* CasillaObjetivo)
     )
    )
  )
;; Funciones para los movimientos de los nodos
(defun obtenerMovimientosNodo (Nodo)
  (let ((listaMovimientosValidos ()))
      ;movimientos basicos
      ;Se pueden obtener mediante la diferencia de la variable global y la información almacenada en el laberinto
      (setq listaMovimientosValidos (append (movimientosBasicos Nodo) listaMovimientosValidos))
      ;movimientos diagonales
      (setq listaMovimientosValidos (append listaMovimientosValidos (moverNoreste Nodo)))
      (setq listaMovimientosValidos (append listaMovimientosValidos (moverNoroeste Nodo)))
      (setq listaMovimientosValidos (append listaMovimientosValidos (moverSureste Nodo)))
      (setq listaMovimientosValidos (append listaMovimientosValidos (moverSuroeste Nodo)))
    )
  )
;movimientos Basicos
(defun movimientosBasicos (Nodo &optional (algoritmo nil))
  (let ((obstaculos nil) (movimientosValidos nil) (listaNodosValidos nil) (renglon 0) (columna 0))
      (setq obstaculos (aref *Laberinto* (first (Nodo-posicionNodo Nodo)) (second (Nodo-posicionNodo Nodo))))   ;obtengo los movimientos
      (setq movimientosValidos (set-difference *PosicionObstaculos* obstaculos :test 'equal))
      (setq renglon (first (Nodo-posicionNodo Nodo)))
      (setq columna (second (Nodo-posicionNodo Nodo)))
      (loop for movimiento in movimientosValidos do 
          (cond
            ((equal movimiento 1);se crea un nodo y se añade a la lista de nodos validos y se regresa la lista
             (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list (- renglon 1) columna) algoritmo) 
                                                 'SinVisitar 
                                                 'Norte 
                                                 (list (- renglon 1) columna))
                                               ) 
                                             listaNodosValidos))
             )
            ((equal movimiento 3)
             (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list (+ renglon 1) columna) algoritmo) 
                                                 'SinVisitar 
                                                 'Sur 
                                                 (list (+ renglon 1) columna))
                                               ) 
                                             listaNodosValidos))
             )
            ((equal movimiento 2)
             (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list renglon (+ columna 1)) algoritmo) 
                                                 'SinVisitar 
                                                 'Este 
                                                 (list renglon (+ columna 1)))
                                               ) 
                                             listaNodosValidos))
             )
            ((equal movimiento 4)
             (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list renglon (- columna 1)) algoritmo) 
                                                 'SinVisitar 
                                                 'Oeste 
                                                 (list renglon (- columna 1)))
                                               ) 
                                             listaNodosValidos))
             )
            )
        )
      listaNodosValidos
    )
  )
;mover al Noreste
(defun moverNoreste (Nodo &optional (algoritmo nil))
  (let ((obstaculos ()) (renglon 0) (columna 0) (obstaculosNodoActual nil))
    (handler-case
      (progn
          (setq renglon (first (Nodo-posicionNodo Nodo)))       
          (setq columna (second (Nodo-posicionNodo Nodo)))
          (setq obstaculosNodoActual (aref *Laberinto* renglon columna))
          ;en caso de que exista la casilla se asignan los obstaculos que tiene a la variable; de lo contrario se regresa nil
          (setq obstaculos (aref *Laberinto* (- renglon 1) (+ columna 1)))
          (if (or
                (and 
                  (equal (find 2 obstaculosNodoActual) 2)   
                  (not (equal (find 4 obstaculos) 4))) 
                (and 
                  (not (equal (find 2 obstaculosNodoActual) 2))
                  (not (equal (find 3 obstaculos) 3))) 
                (and
                  (not (equal (find 4 obstaculos) 4))
                  (not (equal (find 1 obstaculosNodoActual) 1))) 
                (and 
                  (equal (find 1 obstaculosNodoActual) 1) 
                  (not (equal (find 3 obstaculos) 3))) 
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (- renglon 1) (+ columna 1)) algoritmo)
                  'SinVisitar
                  'Noreste
                  (list (- renglon 1) (+ columna 1))
                  )
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => Noreste "))
          nil
        )
      )
    )
  )
;mover al Noroeste
(defun moverNoroeste (Nodo &optional (algoritmo nil))
  (let ((obstaculos ()) (renglon 0) (columna 0) (obstaculosNodoActual nil))
    (handler-case
      (progn
          (setq renglon (first (Nodo-posicionNodo Nodo)))       
          (setq columna (second (Nodo-posicionNodo Nodo)))
          (setq obstaculosNodoActual (aref *Laberinto* renglon columna))
          ;en caso de que exista la casilla se asignan los obstaculos que tiene a la variable; de lo contrario se regresa nil
          (setq obstaculos (aref *Laberinto* (- renglon 1) (- columna 1)))
          (if (or
                (and 
                  (equal (find 4 obstaculosNodoActual) 4)   
                  (not (equal (find 2 obstaculos) 2))) 
                (and 
                  (not (equal (find 4 obstaculosNodoActual) 4))
                  (not (equal (find 3 obstaculos) 3))) 
                (and
                  (not (equal (find 2 obstaculos) 2))
                  (not (equal (find 1 obstaculosNodoActual) 1))) 
                (and 
                  (equal (find 1 obstaculosNodoActual) 1) 
                  (not (equal (find 3 obstaculos) 3))) 
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (- renglon 1) (- columna 1)) algoritmo)
                  'SinVisitar 
                  'Noroeste
                  (list (- renglon 1) (- columna 1))
                  )
                  
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => Noroeste "))
          nil
        )
      )
    )
  )
;mover al sureste
(defun moverSureste (Nodo  &optional (algoritmo nil))
  (let ((obstaculos ()) (renglon 0) (columna 0) (obstaculosNodoActual nil))
    (handler-case
      (progn
          (setq renglon (first (Nodo-posicionNodo Nodo)))       
          (setq columna (second (Nodo-posicionNodo Nodo)))
          (setq obstaculosNodoActual (aref *Laberinto* renglon columna))
          ;en caso de que exista la casilla se asignan los obstaculos que tiene a la variable; de lo contrario se regresa nil
          (setq obstaculos (aref *Laberinto* (+ renglon 1) (+ columna 1)))
          ;(and (not (equal (find 'Oeste obstaculos) 'Oeste)) (not (equal (find 'Norte obstaculos) 'Norte)))
          (if (or
                (and 
                  (equal (find 2 obstaculosNodoActual) 2)   
                  (not (equal (find 4 obstaculos) 4))) 
                (and 
                  (not (equal (find 2 obstaculosNodoActual) 2))
                  (not (equal (find 1 obstaculos) 1))) 
                (and
                  (not (equal (find 4 obstaculos) 4))
                  (not (equal (find 2 obstaculosNodoActual) 2))) 
                (and 
                  (equal (find 2 obstaculosNodoActual) 2) 
                  (not (equal (find 1 obstaculos) 1))) 
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (+ renglon 1) (+ columna 1)) algoritmo)
                  'SinVisitar 
                  'Sureste
                  (list (+ renglon 1) (+ columna 1))
                  )
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => Sureste "))
          nil
        )
      )
    )
  )
;mover al suroeste
(defun moverSuroeste (Nodo &optional (algoritmo nil))
  (let ((obstaculos ()) (renglon 0) (columna 0) (obstaculosNodoActual nil))
    (handler-case
      (progn
          (setq renglon (first (Nodo-posicionNodo Nodo)))       
          (setq columna (second (Nodo-posicionNodo Nodo)))
          ;se obtienen los obstaculos actuales
          (setq obstaculosNodoActual (aref *Laberinto* renglon columna))
          ;en caso de que exista la casilla se asignan los obstaculos que tiene a la variable; de lo contrario se regresa nil
          (setq obstaculos (aref *Laberinto* (+ renglon 1) (- columna 1)))
          ;(or (not (equal (find 'Este obstaculos) 'Este)) (not (equal (find 'Norte obstaculos) 'Norte)))
          #|
          (or
            (and 
              (equal (find 2 obstaculosNodoActual) 2) 
              (not (equal (find 4 obstaculos) 4))) 
            (and 
              (not (equal (find 2 obstaculosNodoActual) 2))
              (not (equal (find 1 obstaculos) 1))) 
            (and
              (not (equal (find 4 obstaculos) 4))
              (not (equal (find 3 obstaculosNodoActual) 3))) 
            (and 
              (equal (find 3 obstaculosNodoActual) 3) 
              (not (equal (find 1 obstaculos) 1))) 
              )
          |#
             
          ;(and (not (equal (find 'Este obstaculos) 'Este)) (not (equal (find 'Norte obstaculos) 'Norte)))
          (if (or
                (and 
                  (equal (find 'Este obstaculosNodoActual) 'Este) 
                  (not (equal (find 'Oeste obstaculos) 'Oeste))) 
                (and 
                  (not (equal (find 'Este obstaculosNodoActual) 'Este))
                  (not (equal (find 'Norte obstaculos) 'Norte))) 
                (and
                  (not (equal (find 'Oeste obstaculos) 'Oeste))
                  (not (equal (find 'Sur obstaculosNodoActual) 'Sur))) 
                (and 
                  (equal (find 'Sur obstaculosNodoActual) 'Sur) 
                  (not (equal (find 'Norte obstaculos) 'Norte))) 
              ) 
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (+ renglon 1) (- columna 1)) algoritmo)
                  'SinVisitar 
                  'Suroeste
                  (list (+ renglon 1) (- columna 1))
                  )
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => Suroeste "))
          nil
        )
      )
    )
  )
;;==================================================================================
;;Pruebas
;;==================================================================================
(defvar nodoPrueba (crearNodo nil 7 'Visitado nil (list 5 4)))
;(defvar nodoPrueba2 (crearNodo  nil 71 nil nil))
;(print nodoPrueba)
;(agregarNodoListaAbierta nodoPrueba 'queue)
;(agregarNodoListaAbierta nodoPrueba2 'queue)
;(print *openList*)
;(setf *openList* (remove-if #'(lambda (x) (equal x nodoPrueba)) *openList*))
;(print (numberp (position nodoPrueba *openList*)))
;(agregarNodolistaCerrada nodoPrueba)
;(print '------------------------)
;(print *openList*)
;(print *Laberinto*)
(read-datafile (string "~/workspace/sample/laberintoPrueba.txt"))
;(print (aref *Laberinto* 5 4))
(print (obtenerMovimientosNodo nodoPrueba))
;(print *Laberinto*)
;(print *closedList*)

;(defvar listaTest '(43 2 1 3 4 5 6 7 8))
;(print listaTest)
#|
(while (> (length listaTest) 0)
 (cond 
   ((equal (first listaTest) 1)
      (setq listaTest (append (list 27 77 07 17 127) (rest listaTest)))
    )
   (T
      (print (first listaTest))
      (setq listaTest (rest listaTest))
     )
   )
 )
|#