;;===============================================================================
;;===============================================================================
;;===============================================================================
;;DECLARACION DE LAS CONSTANTES A USAR EN TODAS LAS BUSQUEDAS
;;===============================================================================
;;===============================================================================
;;===============================================================================
(defvar *contadorNodo* 0)
(defvar *closedList* nil)
(defvar *openList* nil)
(defvar *closedList* nil)
(defvar *NivelDelArbol* 0)
(defvar *EstadosNodos* '(SinVisitar Visitado EnEspera))
(defvar *Solucion* nil)
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
      ;;Se obtiene la informacion de la entrada del laberinto y su salida =========== OPCIONAL
      ;;lectura en falso
      (read-line stream nil nil)
      ;;lectura de las entradas y salidas del laberinto
      (setq *Entrada* (list (read stream nil nil) (read stream nil nil)));;Se leen los dos valores
      ;(setq *Entrada* (read stream nil nil))
      (setq *Salida* (list (read stream nil nil) (read stream nil nil)));;Se leen los dos valores
      ;(setq *Salida* (read stream nil nil))
      (setf *Laberinto* (adjust-array *Laberinto* (list renglones columnas)))
      ;;lectura en falso
      (read-line stream nil nil)
      ;; se carga la información en el arreglo
      (loop for renglon from 0 below renglones do
        (loop for columna from 0 below columnas do
          (setf (aref *Laberinto* renglon columna) (read stream nil nil))
          )
        )
      )
    )
  )
;==============================================================================
;==============================================================================
;==============================================================================
;==============================================================================
;==============================================================================
;
;funciones de utileria general para el manejo de los arboles/grafos de busqueda
;
;==============================================================================
;==============================================================================
;==============================================================================
;==============================================================================
;==============================================================================
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
;==============================================================================
(defstruct Nodo idNodo ancestroNodo costoNodo estadoNodo accionNodo posicionNodo nivelNodo)
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
(defun obtenerNodo (lista idNodoABuscar)
  (let ((nodoEncontrado nil))
    (loop for nodo in lista do
        (if 
          (equal (Nodo-idNodo nodo) idNodoABuscar)
          (setq nodoEncontrado nodo)
          )
      )
      nodoEncontrado
    )
  )
(defun crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)) (nivel 0))
  ; El id se auto incrementa en 1 cada que se crea un nodo nuevo
  ; El ancestro hace referencia al id del nodo de donde se llego al nodo actual
  ; El costo estará dado por la heuristica (Best First Search, A*, para otros algoritmos)
  ; El estado será en espera, visitado, sin visitar
  ; La accion estará denotada por el paso que se tuvo que realizar para llegar al nodo
  ; La posicion del nodo estara dado en coordenadas (renglon columna) para poder ubicarlo en el laberinto
  (incf *contadorNodo* 1)
  (make-Nodo :idNodo *contadorNodo* :ancestroNodo ancestro :costoNodo costo :estadoNodo estado :accionNodo accion :posicionNodo posicion :nivelNodo nivel)
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
  ;(setf *openList* (delete-if #'(lambda (x) (equal x nodo)) *openList*)) ;;Se quita la opción por que se eliminaba al nodo de la lista abierta, cosa que hace el algoritmo en sí
  )
(defun valoraCosto (NodoPadre CasillaObjetivo &optional (algoritmo nil) )
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
      listaMovimientosValidos
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
             ;;Se checa si existe una posible excepcion
             (handler-case
              (progn
                 (aref *Laberinto* (- renglon 1) columna);;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                 (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list (- renglon 1) columna) algoritmo) 
                                                 'SinVisitar 
                                                 'arriba 
                                                 (list (- renglon 1) columna))
                                               ) 
                                             listaNodosValidos)) 
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (print (string "Error: No se puede acceder a esa posicion => arriba"))
                  nil
                )
              )
             )
            ((equal movimiento 3)
             (handler-case
              (progn
                 (aref *Laberinto* (+ renglon 1) columna);;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                  (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list (+ renglon 1) columna) algoritmo) 
                                                 'SinVisitar 
                                                 'abajo 
                                                 (list (+ renglon 1) columna))
                                               ) 
                                             listaNodosValidos))   
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (print (string "Error: No se puede acceder a esa posicion => abajo"))
                  nil
                )
              )
             )
            ((equal movimiento 2)
             (handler-case
              (progn
                  (aref *Laberinto* renglon (+ columna 1));;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                  (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list renglon (+ columna 1)) algoritmo) 
                                                 'SinVisitar 
                                                 'derecha 
                                                 (list renglon (+ columna 1)))
                                               ) 
                                             listaNodosValidos))   
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (print (string "Error: No se puede acceder a esa posicion => derecha"))
                  nil
                )
              )
             )
            ((equal movimiento 4)
             (handler-case
              (progn
                  (aref *Laberinto* renglon (- columna 1));;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                  (setq listaNodosValidos (append (list 
                                               (crearNodo 
                                                 (Nodo-idNodo Nodo) 
                                                 (valoraCosto Nodo (list renglon (- columna 1)) algoritmo) 
                                                 'SinVisitar 
                                                 'izquierda 
                                                 (list renglon (- columna 1)))
                                               ) 
                                             listaNodosValidos))   
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (print (string "Error: No se puede acceder a esa posicion => izquierda"))
                  nil
                )
              )
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
                  (not (equal (find 1 obstaculosNodoActual) 1))
                  (not (equal (find 4 obstaculos) 4))
                  )
                (and
                  (not (equal (find 2 obstaculosNodoActual) 2))
                  (not (equal (find 3 obstaculos) 3))
                  )
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (- renglon 1) (+ columna 1)) algoritmo)
                  'SinVisitar
                  'arriba-derecha
                  (list (- renglon 1) (+ columna 1))
                  )
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => arriba-derecha "))
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
                  (not (equal (find 1 obstaculosNodoActual) 1))
                  (not (equal (find 2 obstaculos) 2))
                  )
                (and
                  (not (equal (find 4 obstaculosNodoActual) 4))
                  (not (equal (find 3 obstaculos) 3))
                  )
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (- renglon 1) (- columna 1)) algoritmo)
                  'SinVisitar 
                  'arriba-izquierda
                  (list (- renglon 1) (- columna 1))
                  )
                  
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => arriba-izquierda "))
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
                  (not (equal (find 3 obstaculosNodoActual) 3))
                  (not (equal (find 4 obstaculos) 4))
                  )
                (and
                  (not (equal (find 2 obstaculosNodoActual) 2))
                  (not (equal (find 1 obstaculos) 1))
                  )
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (+ renglon 1) (+ columna 1)) algoritmo)
                  'SinVisitar 
                  'abajo-derecha
                  (list (+ renglon 1) (+ columna 1))
                  )
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => abajo-derecha "))
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
          (if (or
                (and 
                  (not (equal (find 4 obstaculosNodoActual) 4))
                  (not (equal (find 1 obstaculos) 1))
                  )
                (and
                  (not (equal (find 3 obstaculosNodoActual) 3))
                  (not (equal (find 2 obstaculos) 2))
                  )
              )
              (list 
                (crearNodo 
                  (Nodo-idNodo Nodo) 
                  (valoraCosto Nodo (list (+ renglon 1) (- columna 1)) algoritmo)
                  'SinVisitar 
                  'abajo-izquierda
                  (list (+ renglon 1) (- columna 1))
                  )
                )
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (print (string "Error: No se puede acceder a esa posicion => abajo-izquierda "))
          nil
        )
      )
    )
  )
(defun isEmpty (lista) 
  (cond 
    ((equal (length lista) 0) ;;lista nula o vacia
     T                        ;;Se regresa el valor de verdad por que si esta vacia
     )
    (T
      nil                     ;;Se regresa nil por que no esta vacia la lista
    )
  )
)
;==================================================================================
(defun rastreaSolucion (NodoFinal)
  (if 
    (not (equal (Nodo-ancestro NodoFinal) nil)) ;; Aun no se llega al nodo origen
    (append *Solucion* (Nodo-accion NodoFinal) (rastreaSolucion (obtenerNodo *closedList* (Nodo-ancestro NodoFinal))))
    )
  )
;;==================================================================================
;;Algoritmos de Busqueda
;;==================================================================================
(defun breadth_first_search (Fin)
  (let ((nodoInicial nil) (nodoAux nil) (nodoHijo nil) (movimientosDelNodo nil))
    ;(defun crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)) (nivel 0))
    (setq nodoInicial (crearNodo nil 0 'SinVisitar nil *Entrada* *NivelDelArbol*)) ;;Se obtiene el nodo inicial 
    (agregarNodoListaAbierta nodoInicial 'queue);;Se anade el nodo a la frontera de busqueda 
    (cond
      ((equal (Nodo-posicionNodo nodoInicial) (Nodo-posicionNodo Fin));El nodo que evaluo es igual al nodo destino??? entonces rastrear la solucion
       (rastreaSolucion Fin)
       )
      (T ;;Se procede a explorar el arbol de posibilidades
         (while 
              (not (equal (length *openList*) 0))
                (if 
                  (isEmpty *openList*)
                    ;(return-from breadth_first_search nil) ;;No se encontro la solucion
                    (print (string "No Se encontró la solución"))
                  )
                ;;Se procede con la ejecucion del algoritmo
                (setq nodoAux (pop *openList*)) ;;Se saca el nodo de la lista abierta 
                ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)   
                ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)   
                ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)   
                ;(print (first *openList*))
                ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)   
                ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)   
                ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)   
                (agregarNodoListaCerrada nodoAux);;Se aniade el nodo de la lista cerrada
                (setq movimientosDelNodo (obtenerMovimientosNodo nodoAux))
                (loop for movimiento in movimientosDelNodo do
                      ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)
                      ;(print movimiento)
                      ;(print '-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/)
                      (setq nodoHijo movimiento)
                      (if
                         (not 
                           (or
                             (equal (find nodoHijo *openList*) nodoHijo)
                             (equal (find nodoHijo *closedList*) nodoHijo) 
                             ))
                         (if 
                           (equal (Nodo-posicionNodo nodoHijo) (Nodo-posicionNodo Fin)) ;;Se llego a una solución
                           (rastreaSolucion Fin)
                           )
                        )
                      (agregarNodoListaAbierta nodoHijo 'queue)
                  )
           )
        )
      )
    )
  )
;;==================================================================================
;;Pruebas
;;==================================================================================
;(defvar nodoPrueba2 (crearNodo  nil 71 nil nil))
;(print nodoPrueba)

;(agregarNodoListaAbierta nodoPrueba2 'queue)
;(print *openList*)
;(setf *openList* (remove-if #'(lambda (x) (equal x nodoPrueba)) *openList*))
;(print (numberp (position nodoPrueba *openList*)))
;(agregarNodolistaCerrada nodoPrueba)
;(print *openList*)
;(print *Laberinto*)
(read-datafile (string "./laberintoCodificado.txt"))
;(print *Entrada*)
;(print *Salida*)
(defvar nodoPrueba (crearNodo nil 7 'SinVisitar nil *Salida*))
;(agregarNodoListaAbierta nodoPrueba 'queue)
;(print *openList*)
;(print '-----------------------------------------------------------------------------------)
;(print '-----------------------------------------------------------------------------------)
;(print '-----------------------------------------------------------------------------------)
;(print nodoPrueba)
;(print '-----------------------------------------------------------------------------------)
;(print '-----------------------------------------------------------------------------------)
;(print (aref *Laberinto* 5 4))
(print (breadth_first_search nodoPrueba))
(print *Solucion*)