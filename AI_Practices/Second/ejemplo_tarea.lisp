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
(defvar *NivelDelArbol* 0)
(defvar *MaximoNivel* 0)
(defvar *EstadosNodos* '(SinVisitar Visitado EnEspera))
(defvar *Solucion* nil)
(defvar *debug* nil)
(defvar *diagonales* T)
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
      (if (not (null *debug*))
          (print (string "No se puede obtener la distancia manhattan, los vectores deben tener las mismas dimensiones"))
          )
      0
      )
    )
  )
(defun limpiarListas ()
    (setf *contadorNodo* 0)
    (setf *closedList* nil)
    (setf *openList* nil)
    (setf *NivelDelArbol* 0)
    (setf *Solucion* nil)
   )
(defun actualizaNivel (nivel)
  (cond 
    ((> nivel *MaximoNivel*)
     (setf *MaximoNivel* nivel)
     )
    )
  )
;==============================================================================
(defstruct Nodo idNodo ancestroNodo costoNodo estadoNodo accionNodo posicionNodo nivelNodo)
(defun append-if (&key elemento lista)
  ;(cond
  ;  ((not (obtenerNodoPorPosicion elemento lista))
      ;(append (list elemento) lista)
      (cons elemento lista)
   ;   )
   ; )
  )
(defun prepend-if (&key elemento lista)
  ;(cond
  ;  ((not (obtenerNodoPorPosicion elemento lista))
      (append lista (list elemento))
  ;    )
   ; )
  )
(defun copiarLaberinto (debug)
  (setf *Laberinto* (adjust-array *Laberinto* (list (get-maze-rows) (get-maze-cols))))
  (loop for renglon from 0 below (get-maze-rows) do
    (loop for columna from 0 below (get-maze-cols) do
      (setf (aref *Laberinto* renglon columna) (get-maze renglon columna))
      )
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
  (actualizaNivel (+ nivel 1))
  (make-Nodo :idNodo *contadorNodo* :ancestroNodo ancestro :costoNodo costo :estadoNodo estado :accionNodo accion :posicionNodo posicion :nivelNodo (+ nivel 1))
  )
(defun agregarNodoListaAbierta (nodo &optional &key modo tipoOrdenamiento)
  (cond
    ((equal modo 'stack)
      (setf *openList* (append-if :lista *openList* :elemento nodo )) 
    )
    (T
      (setf *openList* (prepend-if :lista *openList* :elemento nodo))
      )
   )
  (cond
    ((equal tipoOrdenamiento 'costoNodo)
      (setf *openList* (sort *openList* #'< :key #'Nodo-costoNodo))   
     )
    ;(T
    ; (setf *openList* (sort *openList* #'< :key #'Nodo-idNodo))
    ; )
    )
 )
(defun agregarNodoListaCerrada(nodo)
  (setq *closedList* (append *closedList* (list nodo)))
  ;(setf *openList* (delete-if #'(lambda (x) (equal x nodo)) *openList*)) ;;Se quita la opción por que se eliminaba al nodo de la lista abierta, cosa que hace el algoritmo en sí
  )
(defun valoraCosto (NodoPadre CasillaObjetivo &optional (algoritmo nil) )
  (cond
    ((null algoritmo)
     (+ (Nodo-nivelNodo NodoPadre) 1) ;No hay algoritmo a evaluar
     )
    ((equal algoritmo 'A*)
     ;Se calcula utilizando f(x) = g(x) + H(x)
     (+ (Nodo-nivelNodo NodoPadre) (distanciaManhattan *Salida* CasillaObjetivo) )
     )
    ((equal algoritmo 'Best)
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
      ;movimientos diagonales*
      (cond
        (*diagonales*
         (setq listaMovimientosValidos (append listaMovimientosValidos (moverNoreste Nodo)))
         (setq listaMovimientosValidos (append listaMovimientosValidos (moverNoroeste Nodo)))
         (setq listaMovimientosValidos (append listaMovimientosValidos (moverSureste Nodo)))
         (setq listaMovimientosValidos (append listaMovimientosValidos (moverSuroeste Nodo))) 
         )
        )
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
                 (setq listaNodosValidos (append (list (list (list (- renglon 1) columna) 'arriba)) listaNodosValidos)) 
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (if (not (null *debug*))
                      (print (string "Error: No se puede acceder a esa posicion => arriba"))
                      )
                  nil
                )
              )
             )
            ((equal movimiento 2)
             (handler-case
              (progn
                  (aref *Laberinto* renglon (+ columna 1));;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                  (setq listaNodosValidos (append (list (list (list renglon (+ columna 1)) 'derecha ))  listaNodosValidos))   
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (if (not (null *debug*))
                      (print (string "Error: No se puede acceder a esa posicion => derecha"))
                      )
                  
                  nil
                )
              )
             )
            ((equal movimiento 3)
             (handler-case
              (progn
                 (aref *Laberinto* (+ renglon 1) columna);;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                  (setq listaNodosValidos (append (list (list (list (+ renglon 1) columna) 'abajo)) listaNodosValidos))   
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (if (not (null *debug*))
                      (print (string "Error: No se puede acceder a esa posicion => abajo"))
                      )
                  nil
                )
              )
             )            
            ((equal movimiento 4)
             (handler-case
              (progn
                  (aref *Laberinto* renglon (- columna 1));;Si no Hay excepcion significa que puede seguir con la siguiente linea de codigo 
                  (setq listaNodosValidos (append (list (list (list renglon (- columna 1)) 'izquierda  )) listaNodosValidos))   
                )
                (error (e) 
                  ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
                  (if (not (null *debug*))
                    (print (string "Error: No se puede acceder a esa posicion => izquierda"))
                  )
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
              (list (list (list (- renglon 1) (+ columna 1)) 'arriba-derecha))
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (if (not (null *debug*))
            (print (string "Error: No se puede acceder a esa posicion => arriba-derecha "))
          )
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
              (list (list (list (- renglon 1) (- columna 1)) 'arriba-izquierda))
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (if (not (null *debug*))
            (print (string "Error: No se puede acceder a esa posicion => arriba-izquierda "))
            )
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
              (list (list (list (+ renglon 1) (+ columna 1)) 'abajo-derecha ))
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (if (not (null *debug*))
            (print (string "Error: No se puede acceder a esa posicion => abajo-derecha "))
            )
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
              (list (list (list (+ renglon 1) (- columna 1))  'abajo-izquierda ))
              ) 
        )
        (error (e) 
          ;nil; no puede realizarse el movimiento por que se sale del arreglo (laberinto)
          (if (not (null *debug*))
            (print (string "Error: No se puede acceder a esa posicion => abajo-izquierda "))
            )
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
(defun obtenerNodo (lista idNodoABuscar)
  (let ((nodoEncontrado nil))
    (loop for nodo in lista do
          (if (equal (Nodo-idNodo nodo) idNodoABuscar)
              (return-from obtenerNodo nodo)
              )
      )
      
    )
  )
(defun obtenerNodoPorPosicion (posicion lista &optional (regresaEstructura nil))
  (let ((nodoEncontrado nil))
    (loop for nodo in lista do
        (cond 
          ((equal (Nodo-posicionNodo nodo) posicion)
           ;(print 'Se_ENCONTRO_POR_POSICION)
           (if regresaEstructura
                (return-from obtenerNodoPorPosicion nodo)
                (return-from obtenerNodoPorPosicion T)    
               )
           )          
        )
      )
    (return-from obtenerNodoPorPosicion nil)
    )
  )
(defun rastreaSolucion (nodoARastrear)
    (setq *Solucion* (append *Solucion* (list (Nodo-accionNodo nodoARastrear))))
    (print (Nodo-accionNodo nodoARastrear))
    (let ((bandera T) (nodoAux nil))
      (setq nodoAux (obtenerNodo *closedList* (Nodo-ancestroNodo nodoARastrear)))
      ;(print nodoAux)
      ;(print '------------------------)
      (while 
        (not (equal (Nodo-ancestroNodo nodoAux) nil))
          (setq *Solucion* (append *Solucion* (list (Nodo-accionNodo nodoAux))))    
          (print (Nodo-accionNodo nodoAux))
          (setq nodoAux (obtenerNodo *closedList* (Nodo-ancestroNodo nodoAux)))          
        )
      )
  )
;;==================================================================================
;;Algoritmos de Busqueda
;;==================================================================================

;;
;;  BUSQUEDA EN AMPLITUD
;;
(defun breadth_first_search (nodoDestino nodoInicial)
  ;(agregarNodoListaAbierta (crearNodo nil 0 'SinVisitar nil *Entrada* *NivelDelArbol*) :modo'queue);;Se anade el nodo inicial a la frontera de busqueda   
  (agregarNodoListaAbierta nodoInicial :modo 'queue)
  (let ((nodoAux nil) (movimientosValidos nil)) 
    (while (not (null *openList*))
      (setq nodoAux (pop *openList*)) ;;se obtiene el único elemento de la lista abierta
      (setf (Nodo-estadoNodo nodoAux) 'Visitado) ;;Se cambia el estado del nodo
      (agregarNodoListaCerrada nodoAux);;El nodo que fue expandido se agrega a la lista cerrada   
      (setq movimientosValidos (obtenerMovimientosNodo nodoAux));;se obtienen los movimientos basicos del nodo
      (cond
        ((equal (Nodo-posicionNodo nodoAux) (Nodo-posicionNodo nodoDestino))
         (setf *closedList* (sort *closedList* #'< :key #'Nodo-idNodo))
         (rastreaSolucion nodoAux)
          (return-from breadth_first_search *Solucion*)
         )
        (T
          ;;
          (loop for movimiento in movimientosValidos do
            (cond
              ((and (not (obtenerNodoPorPosicion (first movimiento) *openList*)) (not (obtenerNodoPorPosicion (first movimiento) *closedList*))) 
                  ;crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)) (nivel 0))
                  (agregarNodoListaAbierta 
                    (crearNodo 
                      (Nodo-idNodo nodoAux) 
                      (valoraCosto nodoAux (first movimiento))
                      'SinVisitar
                      (second movimiento)
                      (first movimiento)
                      (Nodo-nivelNodo nodoAux)
                       ) 
                    :modo 'queue
                    )
                  ;(setf *openList* (sort *openList* #'< :key #'Nodo-idNodo))   
                )
              )
            )
          )
        )
      )
    )
  )
;;
;;  BUSQUEDA EN PROFUNDIDAD
;;
(defun depth_first_search (nodoDestino nodoInicial)
  (agregarNodoListaAbierta nodoInicial :modo 'queue);;Se anade el nodo inicial a la frontera de busqueda        
  (let ((nodoAux nil) (movimientosValidos nil)) 
    (while (not (null *openList*))
      (setq nodoAux (pop *openList*)) ;;se obtiene el único elemento de la lista abierta
      (setf (Nodo-estadoNodo nodoAux) 'Visitado) ;;Se cambia el estado del nodo
      (agregarNodoListaCerrada nodoAux);;El nodo que fue expandido se agrega a la lista cerrada   
      (setq movimientosValidos (obtenerMovimientosNodo nodoAux));;se obtienen los movimientos basicos del nodo
      (cond
        ((equal (Nodo-posicionNodo nodoAux) (Nodo-posicionNodo nodoDestino))
         (setf *closedList* (sort *closedList* #'< :key #'Nodo-idNodo))
           (rastreaSolucion nodoAux)
           (return-from depth_first_search *Solucion*)
         )
        (T
          ;;
          (loop for movimiento in movimientosValidos do
            (cond
              ((and (not (obtenerNodoPorPosicion (first movimiento) *openList*)) (not (obtenerNodoPorPosicion (first movimiento) *closedList*))) 
                  ;crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)) (nivel 0))
                  (agregarNodoListaAbierta 
                    (crearNodo 
                      (Nodo-idNodo nodoAux) 
                      (valoraCosto nodoAux (first movimiento))
                      'SinVisitar
                      (second movimiento)
                      (first movimiento)
                      (Nodo-nivelNodo nodoAux)
                       ) 
                    :modo 'stack
                    )
                )
              )
            )
          )
        )
      )
    )
  )
;;
;; BEST FIRST 
;;
(defun best_first_search (nodoDestino nodoInicial)
  ;(agregarNodoListaAbierta (crearNodo nil 0 'SinVisitar nil *Entrada* *NivelDelArbol*) :modo'queue);;Se anade el nodo inicial a la frontera de busqueda   
  (agregarNodoListaAbierta nodoInicial :modo 'queue)
  (let ((nodoAux nil) (movimientosValidos nil) (nodoActual nil) (nodoAnterior nil)) 
    (while (not (null *openList*))
      (setq nodoAux (pop *openList*)) ;;se obtiene el único elemento de la lista abierta
      (setf (Nodo-estadoNodo nodoAux) 'Visitado) ;;Se cambia el estado del nodo
      (agregarNodoListaCerrada nodoAux);;El nodo que fue expandido se agrega a la lista cerrada   
      (setq movimientosValidos (obtenerMovimientosNodo nodoAux));;se obtienen los movimientos basicos del nodo
      (cond
        ((equal (Nodo-posicionNodo nodoAux) (Nodo-posicionNodo nodoDestino))
         (setf *closedList* (sort *closedList* #'< :key #'Nodo-idNodo))
         (rastreaSolucion nodoAux)
          (return-from best_first_search *Solucion*)
         )
        (T
          ;;
          (loop for movimiento in movimientosValidos do
            (cond
              ((obtenerNodoPorPosicion (first movimiento) *openList*)
               ;;actualizo el nodo si su costo es menor
                (setf nodoAnterior (obtenerNodoPorPosicion (first movimiento) *openList* T));;el que se encuentra en la lista abierta actualmente
                (setf nodoActual (crearNodo 
                      (Nodo-idNodo nodoAux) 
                      (valoraCosto nodoAux (first movimiento) 'Best)
                      'SinVisitar
                      (second movimiento)
                      (first movimiento)
                      (Nodo-nivelNodo nodoAux)
                       ))
                (cond
                  ((< (Nodo-costoNodo nodoActual) (Nodo-costoNodo nodoAnterior))
                   ;;Actualizo
                   (setf *openList* (delete-if #'(lambda (x) (equal x nodoAnterior)) *openList*))
                   ;;Se agrega el nodo con camino más corto a la lista abierta
                   (agregarNodoListaAbierta nodoActual :modo 'queue)
                   (setf *openList* (sort *openList* #'< :key #'Nodo-costoNodo))
                   )
                  )
               )
              ((and (not (obtenerNodoPorPosicion (first movimiento) *openList*)) (not (obtenerNodoPorPosicion (first movimiento) *closedList*))) 
                  ;crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)) (nivel 0))
                  (agregarNodoListaAbierta 
                    (crearNodo 
                      (Nodo-idNodo nodoAux) 
                      (valoraCosto nodoAux (first movimiento) 'Best)
                      'SinVisitar
                      (second movimiento)
                      (first movimiento)
                      (Nodo-nivelNodo nodoAux)
                       ) 
                    :modo 'queue
                    )
                  (setf *openList* (sort *openList* #'< :key #'Nodo-costoNodo))   
                )
              )
            )
          )
        )
      )
    )
  )
;;
;; A star
;;
(defun a_star_search (nodoDestino nodoInicial)
  ;(agregarNodoListaAbierta (crearNodo nil 0 'SinVisitar nil *Entrada* *NivelDelArbol*) :modo'queue);;Se anade el nodo inicial a la frontera de busqueda   
  (agregarNodoListaAbierta nodoInicial :modo 'queue)
  (let ((nodoAux nil) (movimientosValidos nil) (nodoActual nil) (nodoAnterior nil)) 
    (while (not (null *openList*))
      (setq nodoAux (pop *openList*)) ;;se obtiene el único elemento de la lista abierta
      (setf (Nodo-estadoNodo nodoAux) 'Visitado) ;;Se cambia el estado del nodo
      (agregarNodoListaCerrada nodoAux);;El nodo que fue expandido se agrega a la lista cerrada   
      (setq movimientosValidos (obtenerMovimientosNodo nodoAux));;se obtienen los movimientos basicos del nodo
      (cond
        ((equal (Nodo-posicionNodo nodoAux) (Nodo-posicionNodo nodoDestino))
         (setf *closedList* (sort *closedList* #'< :key #'Nodo-idNodo))
         (rastreaSolucion nodoAux)
          (return-from a_star_search *Solucion*)
         )
        (T
          ;;
          (loop for movimiento in movimientosValidos do
            (cond
              ((obtenerNodoPorPosicion (first movimiento) *openList*)
               ;;actualizo el nodo si su costo es menor
                (setf nodoAnterior (obtenerNodoPorPosicion (first movimiento) *openList* T));;el que se encuentra en la lista abierta actualmente
                (setf nodoActual (crearNodo 
                      (Nodo-idNodo nodoAux) 
                      (valoraCosto nodoAux (first movimiento) 'A*)
                      'SinVisitar
                      (second movimiento)
                      (first movimiento)
                      (Nodo-nivelNodo nodoAux)
                       ))
                (cond
                  ((< (Nodo-costoNodo nodoActual) (Nodo-costoNodo nodoAnterior))
                   ;;Actualizo
                   (setf *openList* (delete-if #'(lambda (x) (equal x nodoAnterior)) *openList*))
                   ;;Se agrega el nodo con camino más corto a la lista abierta
                   (agregarNodoListaAbierta nodoActual :modo 'queue)
                   (setf *openList* (sort *openList* #'< :key #'Nodo-costoNodo))
                   )
                  )
               )
              ((and (not (obtenerNodoPorPosicion (first movimiento) *openList*)) (not (obtenerNodoPorPosicion (first movimiento) *closedList*))) 
                  ;crearNodo (&optional (ancestro nil) (costo 0) (estado nil) (accion nil) (posicion (list 0 0)) (nivel 0))
                  (agregarNodoListaAbierta 
                    (crearNodo 
                      (Nodo-idNodo nodoAux) 
                      (valoraCosto nodoAux (first movimiento) 'A*)
                      'SinVisitar
                      (second movimiento)
                      (first movimiento)
                      (Nodo-nivelNodo nodoAux)
                       ) 
                    :modo 'queue
                    )
                  (setf *openList* (sort *openList* #'< :key #'Nodo-costoNodo))   
                )
              )
            )
          )
        )
      )
    )
  )


;===============================================================
;===============================================================
;  FUNCIONES USADAS POR LA INTERFAZ DE CL MAZE
;===============================================================
;===============================================================

;Depth-first

(defun depth-first (estado-inicial estado-meta)
  (limpiarListas)
  (let ((nodoInicial Nil) (nodoFinal nil) (time1 0)(time2 0))
    (setf time1 (get-internal-run-time))
    (setf nodoInicial (crearNodo nil 0 'SinVisitar nil (list (first estado-inicial) (second estado-inicial)) 0))
    (setf nodoFinal (crearNodo nil 0 'SinVisitar nil (list (first estado-meta) (second estado-meta)) 0))
    ;;Se copia el laberinto por que el de cl-maze me causa dolores de cabeza :3 nada como manejar excepciones
    (copiarLaberinto nil)
    (depth_first_search nodoFinal nodoInicial) 
    (setq time2 (get-internal-run-time))
    ;;
    (set-result-info :algorithm "Depth-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length *Solucion*)
                     :created-nodes *contadorNodo*
                     :explored-nodes (length *closedList*)
                     :max *MaximoNivel*)
    ;;
      (reverse *Solucion*)
    )
  )

;Breadth-First Search

(defun breadth-first (estado-inicial estado-meta)
  (limpiarListas)
  (let ((nodoInicial Nil) (nodoFinal nil) (time1 0)(time2 0))
    (setf time1 (get-internal-run-time))
    (setf nodoInicial (crearNodo nil 0 'SinVisitar nil (list (first estado-inicial) (second estado-inicial)) 0))
    (setf nodoFinal (crearNodo nil 0 'SinVisitar nil (list (first estado-meta) (second estado-meta)) 0))
    ;;Se copia el laberinto por que el de cl-maze me causa dolores de cabeza :3 nada como manejar excepciones
    (copiarLaberinto nil)
    (breadth_first_search nodoFinal nodoInicial) 
    (setq time2 (get-internal-run-time))
    ;;
    (set-result-info :algorithm "Breadth-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length *Solucion*)
                     :created-nodes *contadorNodo*
                     :explored-nodes (length *closedList*)
                     :max *MaximoNivel*)
    ;;
      (reverse *Solucion*)
    )
  )

; Best-first

(defun best-first (estado-inicial estado-meta)
  (limpiarListas)
  (let ((nodoInicial Nil) (nodoFinal nil) (time1 0)(time2 0))
    (setf time1 (get-internal-run-time))
    (setf nodoInicial (crearNodo nil 0 'SinVisitar nil (list (first estado-inicial) (second estado-inicial)) 0))
    (setf nodoFinal (crearNodo nil 0 'SinVisitar nil (list (first estado-meta) (second estado-meta)) 0))
    ;;Se copia el laberinto por que el de cl-maze me causa dolores de cabeza :3 nada como manejar excepciones
    (copiarLaberinto nil)
    (best_first_search nodoFinal nodoInicial) 
    (setq time2 (get-internal-run-time))
    ;;
    (set-result-info :algorithm "Best-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length *Solucion*)
                     :created-nodes *contadorNodo*
                     :explored-nodes (length *closedList*)
                     :max *MaximoNivel*)
    ;;
      (reverse *Solucion*)
    )
  )

; a-star

(defun a-star (estado-inicial estado-meta)
  (limpiarListas)
  (let ((nodoInicial Nil) (nodoFinal nil) (time1 0)(time2 0))
    (setf time1 (get-internal-run-time))
    (setf nodoInicial (crearNodo nil 0 'SinVisitar nil (list (first estado-inicial) (second estado-inicial)) 0))
    (setf nodoFinal (crearNodo nil 0 'SinVisitar nil (list (first estado-meta) (second estado-meta)) 0))
    ;;Se copia el laberinto por que el de cl-maze me causa dolores de cabeza :3 nada como manejar excepciones
    (copiarLaberinto nil)
    (best_first_search nodoFinal nodoInicial) 
    (setq time2 (get-internal-run-time))
    ;;
    (set-result-info :algorithm "A*-First Search"
                     :time (format nil "~4$" (/ (- time2 time1) internal-time-units-per-second))
                     :steps (length *Solucion*)
                     :created-nodes *contadorNodo*
                     :explored-nodes (length *closedList*)
                     :max *MaximoNivel*)
    ;;
      (reverse *Solucion*)
    )
  )
;(tarea:show-maze-interface)