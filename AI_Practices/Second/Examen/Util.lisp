;;==============================================================================
;;==============================================================================
;; Funcion para cargar la base de datos que contendrá los movimientos
;;==============================================================================
;;==============================================================================
(defvar *TablaPosiciones* (make-hash-table :test 'equal))
(defvar *Casillas* (list 0 0 0 0 0 0 0 0 0 0))
(defvar *OpenList* nil)
(defvar *ClosedList* nil)
(defvar *Solucion* nil)
(defvar *ContadorSoluciones* 0)
(defvar *ContadorNodo* 0)
(defvar *ListaMosaicos* nil)
;;==================================================================================
(defmacro while (test &rest body)
  "Repeat body while test is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))
;;==================================================================================
;;===================================inicio del programa============================
;;==================================================================================
;(load "Mosaicos.lisp")
(defstruct
  Mosaico
;;
  idMosaico
  idMosaicoNumerico
  ancestroMosaico
  costoMosaico
  numeroDeRotaciones
  colores
  gradosDeRotacion
  ubicacion)
;===============================================================================
;(defvar *ordenCasillas* (list 3 0 7 4 1 8 5 2 9 6))
(defvar *Casillas* (list 0 0 0 0 0 0 0 0 0 0))
(defun append-if (&key elemento lista)
  (cons elemento lista))
(defun prepend-if (&key elemento lista)
  (append lista (list elemento)))
(defun agregarMosaicoListaAbierta (nodo &optional &key modo tipoOrdenamiento)
  (cond
    ((equal modo 'stack)
      (setf *OpenList* (append-if :lista *OpenList* :elemento nodo )))
    (T
      (setf *OpenList* (prepend-if :lista *OpenList* :elemento nodo)))))
(defun agregarMosaicoListaCerrada(nodo)
  (setq *ClosedList* (append *ClosedList* (list nodo))))
(defun obtenerMosaico (lista idMosaico &optional (regresaEstructura nil))
  (let ((nodoEncontrado nil))
    (loop for mosaico in lista do
           (cond 
                ((equal (Mosaico-idMosaicoNumerico mosaico) idMosaico)
                 (if regresaEstructura
                     (return-from obtenerMosaico mosaico)
                     (return-from obtenerMosaico T))))))) 
(defun obtenerMosaicoPorUbicacion (ubicacion lista &optional (regresaEstructura nil))
  (let ((mosaicoEncontrado nil))
    (loop for mosaico in lista do
      (cond
        ((equal (Mosaico-ubicacion mosaico) ubicacion)
         (if regresaEstructura
           (return-from obtenerMosaicoPorUbicacion mosaico)
           (return-from obtenerMosaicoPorUbicacion T)))))))
(defun obtenerMosaicoPorLetra (idLetraMosaico lista &optional (regresaEstructura nil))
  (loop for elemento in lista do
      (cond
        ((equal (Mosaico-idMosaico elemento) idLetraMosaico)
         (if regresaEstructura
             (return-from obtenerMosaicoPorLetra elemento)
             T)))))
(defun rastreaSolucion (mosaicoARastrear)
    (setf *Solucion* (append *Solucion* (list (list (Mosaico-idMosaico mosaicoARastrear) (Mosaico-gradosDeRotacion mosaicoARastrear)))))
    (let ((bandera T) (nodoAux nil))
      (setq nodoAux (obtenerMosaico *ClosedList* (Mosaico-ancestroMosaico mosaicoARastrear) T))
      (while 
        (not (equal (Mosaico-ancestroMosaico nodoAux) nil))
          (setf *Solucion* (append *Solucion* (list (list (Mosaico-idMosaico nodoAux) (Mosaico-gradosDeRotacion nodoAux)))))    
          (setf nodoAux (obtenerMosaico *ClosedList* (Mosaico-ancestroMosaico nodoAux) T)))
      (print '-----------------------------------------------------------------------)
      (print *Solucion*)
      (limpiarListas nil)
      (incf *ContadorSoluciones* 1)))
;;==================================================================================
(defun limpiarListas (debug)
  (setf *Casillas* (list 0 0 0 0 0 0 0 0 0 0))
  (setf *OpenList* nil)
  (setf *ClosedList* nil)
  (setf *Solucion* nil))
;;==================================================================================
(defun cargaMosaicos (debug)
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-mosaico 
                                :idMosaico 'A
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 1 6 4 5 2) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'B 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(4 3 10 2 1 1) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'C 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(2 4 4 6 6 9) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'D 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(7 6 2 4 3 5) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'E
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0 
                                :colores '(1 4 4 9 2 2) 
                                :gradosDeRotacion 30 
                                :ubicacion '( 0 0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'F 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(4 7 1 1 6 4) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'G 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(6 3 9 8 2 1) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'H 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(10 7 8 7 2 1) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'I 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(10 9 2 10 3 5) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'J 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(2 9 4 4 10 5) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'K 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 9 9 3 4 6) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'L 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(5 10 6 7 9 4) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'M 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(4 3 4 6 7 7) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'N 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 1 6 3 1 8) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'O 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(5 1 7 6 3 4) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'P 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(7 2 9 2 5 8) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'Q 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 6 7 4 1 2) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'R 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 1 6 2 1 4) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'S 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(3 3 1 4 6 8) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (incf *ContadorNodo* 1)
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'T 
                                :idMosaicoNumerico *ContadorNodo*
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(9 4 7 7 1 8) 
                                :gradosDeRotacion 30 
                                :ubicacion '(0)) 
                              *ListaMosaicos*)))
;;==================================================================================
(defun contieneColores (colores listaColores)
  (cond
    ((< (length listaColores) (length colores))
      nil)
    (T
      (cond 
        ((equal 1 (length colores))
     		 (if (equal (list (first listaColores)) colores)
         		T
         		(contieneColores colores (rest listaColores))))
  	    ((equal 2 (length colores))
     		 (if (equal (list (first listaColores) (second listaColores)) colores)
         		T
       			(contieneColores colores (rest listaColores))))
		    ((equal 3 (length colores))
     		 (if (equal (list (first listaColores) (second listaColores) (third listaColores)) colores)
         		T
         		(contieneColores colores (rest listaColores))))))))
;;==================================================================================
(defun encontrarCombinacionColoresEnMosaico (colores mosaico)
  (let ((vecinos))
  	(setf vecinos (Mosaico-colores mosaico))
   	(cond
      ((contieneColores colores vecinos)
       T)
      ((contieneColores colores (append (last vecinos) (butlast vecinos)))
       T)
      (T
        nil))))
;;==================================================================================
(defun cargaTablaPosiciones (path-to-file)
  (let ((colores nil) (mosaicos nil))
    (with-open-file (input-stream path-to-file)
        (setf colores (read input-stream nil nil))
        (loop until (null colores) do
          (loop for mosaico in *ListaMosaicos* do
          	(cond
        	 ((encontrarCombinacionColoresEnMosaico colores mosaico)
              (setf mosaicos (append (list mosaico) mosaicos))))
           )
          (setf (gethash colores *TablaPosiciones*) mosaicos)
          (setf mosaicos nil)
          (setf colores (read input-stream nil nil))))))
;;==================================================================================
(defun obtenerMosaicosConColores (colores)
  (return-from obtenerMosaicosConColores (gethash colores *TablaPosiciones*)))
;;==================================================================================
(defun rotarMosaicoHastaPosicion (mosaico combinacionABuscar posicionCombinacion)
  (let ((bandera nil) (mosaicoAux nil))
    (loop until bandera do 
      (cond
        ((equal 1 (length posicionCombinacion))
         (cond
           ((equal combinacionABuscar (list (nth (- (first posicionCombinacion) 1) (Mosaico-colores mosaico))))
            (setf bandera T))))
        ((equal 2 (length posicionCombinacion));;se tienen que considerar 2 casillas
         (cond
           ((equal combinacionABuscar (list 
                                       (nth (- (first posicionCombinacion) 1) (Mosaico-colores mosaico))
                                       (nth (- (second posicionCombinacion) 1) (Mosaico-colores mosaico))))
            (setf bandera T))))
        ((equal 3 (length posicionCombinacion));;se tienen que considerar 3 casillas
         (cond
           ((equal combinacionABuscar (list 
                                       (nth (- (first posicionCombinacion) 1) (Mosaico-colores mosaico))
                                       (nth (- (second posicionCombinacion) 1) (Mosaico-colores mosaico))
                                       (nth (- (third posicionCombinacion) 1) (Mosaico-colores mosaico))))
            (setf bandera T)))))
        ;;Se procede a rotar ya que no se cumplió ninguna condicion
        ;(print 'SE_CAMBIO_BANDERA)
        (cond 
          ((not bandera)
            (setf (Mosaico-colores mosaico) (append (last (Mosaico-colores mosaico)) (butlast (Mosaico-colores mosaico))))
            ;(incf (Mosaico-numeroDeRotaciones mosaico) 1)
            (setf (Mosaico-numeroDeRotaciones mosaico) (mod (+ (Mosaico-numeroDeRotaciones mosaico) 1) 6))
            (setf (Mosaico-gradosDeRotacion mosaico) (mod (+ (Mosaico-gradosDeRotacion mosaico) 60) 360))
            ;(incf (Mosaico-gradosDeRotacion mosaico) 60)
           )))
    (incf *ContadorNodo* 1)
    (setf (Mosaico-idMosaicoNumerico mosaico) *contadorNodo*)
    (setf mosaicoAux (COPY-STRUCTURE mosaico))
    mosaicoAux))
(defun obtenerTodosLosMovimientos (mosaico)
  (let ((listaMovimientos nil) (mosaicoAux nil))
    ;;
    (setf (Mosaico-colores mosaico) (append (last (Mosaico-colores mosaico)) (butlast (Mosaico-colores mosaico))))
    (incf (Mosaico-numeroDeRotaciones mosaico) 1) 
    (incf (Mosaico-gradosDeRotacion mosaico) 60)
    (setf listaMovimientos (append (list mosaico) listaMovimientos))
    ;;
    (setf mosaicoAux (COPY-STRUCTURE mosaico))
    (setf (Mosaico-colores mosaicoAux) (append (last (Mosaico-colores mosaicoAux)) (butlast (Mosaico-colores mosaicoAux))))
    (incf (Mosaico-numeroDeRotaciones mosaicoAux) 1) 
    (incf (Mosaico-gradosDeRotacion mosaicoAux) 60)
    (setf listaMovimientos (append (list mosaicoAux) listaMovimientos))    
    ;;
    (setf mosaicoAux (COPY-STRUCTURE mosaicoAux))
    (setf (Mosaico-colores mosaicoAux) (append (last (Mosaico-colores mosaicoAux)) (butlast (Mosaico-colores mosaicoAux))))
    (incf (Mosaico-numeroDeRotaciones mosaicoAux) 1) 
    (incf (Mosaico-gradosDeRotacion mosaicoAux) 60)
    (setf listaMovimientos (append (list mosaicoAux) listaMovimientos))
    ;;
    (setf mosaicoAux (COPY-STRUCTURE mosaicoAux))
    (setf (Mosaico-colores mosaicoAux) (append (last (Mosaico-colores mosaicoAux)) (butlast (Mosaico-colores mosaicoAux))))
    (incf (Mosaico-numeroDeRotaciones mosaicoAux) 1) 
    (incf (Mosaico-gradosDeRotacion mosaicoAux) 60)
    (setf listaMovimientos (append (list mosaicoAux) listaMovimientos))
    ;;
    (setf mosaicoAux (COPY-STRUCTURE mosaicoAux))
    (setf (Mosaico-colores mosaicoAux) (append (last (Mosaico-colores mosaicoAux)) (butlast (Mosaico-colores mosaicoAux))))
    (incf (Mosaico-numeroDeRotaciones mosaicoAux) 1) 
    (incf (Mosaico-gradosDeRotacion mosaicoAux) 60)
    (setf listaMovimientos (append (list mosaicoAux) listaMovimientos))
    ;;
    ;(setf mosaicoAux (COPY-STRUCTURE mosaicoAux))
    ;(setf (Mosaico-colores mosaicoAux) (append (last (Mosaico-colores mosaicoAux)) (butlast (Mosaico-colores mosaicoAux))))
    ;(incf (Mosaico-numeroDeRotaciones mosaicoAux) 1) 
    ;(incf (Mosaico-gradosDeRotacion mosaicoAux) 60)
    ;(setf listaMovimientos (append (list mosaicoAux) listaMovimientos))
    ;;
    listaMovimientos))
(defun rotarMosaicosEnListaConCombinacionHastaPosicion (ListaMosaicos combinacionABuscar posicionCombinacion)
  (let ((listaMosaicosRotados nil) (mosaicoCopia nil))
    (loop for mosaico in ListaMosaicos do
      (setf listaMosaicosRotados (append (list (rotarMosaicoHastaPosicion mosaico combinacionABuscar posicionCombinacion)) listaMosaicosRotados)))
    listaMosaicosRotados))
;============================================================================
(defun obtenerColorEnPosicion (mosaico posicion)
  (let ((color))
    (setf color (nth (- posicion 1) (Mosaico-colores mosaico)))
    color))
(defun exiteMosaicoEnCasillas (mosaicoABuscar)
  (loop for mosaico in *Casillas* do
      (if
        (equal mosaico 0)
        (return-from exiteMosaicoEnCasillas nil))
      (if
        (equal (Mosaico-idMosaico mosaico) (Mosaico-idMosaico mosaicoABuscar))
        (return-from exiteMosaicoEnCasillas T)))
  (return-from exiteMosaicoEnCasillas nil))
(defun casillaVacia (casilla)
  (if
    (equal (nth casilla *Casillas*) 0)
    (return-from casillaVacia T)
    (return-from casillaVacia nil)))
;============================================================================
(defun buscarSoluciones (&optional debug)
  (loop for mosaico in *ListaMosaicos* do
    (cond
      ((equal *ContadorSoluciones* 3)
       (return-from buscarSoluciones nil))) ;;ya se encontraron las 3 soluciones indicadas
    (loop for mosaicoRotado in (obtenerTodosLosMovimientos mosaico) do
      ;(print mosaicoRotado)
      (setf (Mosaico-ubicacion mosaicoRotado) 0);;
      (setf (nth 0 *Casillas*) mosaicoRotado);; Se añade a la primer posición
      ;(print '-------------------------------------------------)
      ;(print '-------------------------------------------------)
      ;(print '-------------------------------------------------)
      ;(print *Casillas*)
      ;(print '-------------------------------------------------)
      ;(print '-------------------------------------------------)
      ;(print '-------------------------------------------------)
      (agregarMosaicoListaAbierta mosaicoRotado :modo 'queue)
      (let ((mosaicoAux nil) (mosaicosValidos nil) (mosaicoActual nil) (mosaicoAnterior nil) (mosaicosEnTabla nil) (mosaicoAGuardar nil))
       (loop until (null *OpenList*) do
         (setf mosaicoAux (pop *OpenList*))
         ;(print '********************)
         ;(print mosaicoAux)
         ;(print '********************)
         (agregarMosaicoListaCerrada mosaicoAux)
         ;;procedo a checar que onda con las condiciones
         (cond
           ((equal 0 (Mosaico-ubicacion mosaicoAux)) ;; checo los vecinos que corresponden a la casilla 1, es a la que me moveré!
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list (obtenerColorEnPosicion (nth 0 *Casillas*) 1))))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion mosaicosEnTabla (list (obtenerColorEnPosicion (nth 0 *Casillas*) 1)) (list 4))))
           ((equal 1 (Mosaico-ubicacion mosaicoAux)) ;;checo los vecinos que corresponden a la casilla 2, es a la que me moveré
            ;(print (string "Movimiento hacia casilla 2"))
            ;(print '********************)
            ;(print (obtenerMosaicosConColores (list (obtenerColorEnPosicion (nth 0 *Casillas*) 3))))
            ;(print (obtenerColorEnPosicion (nth 0 *Casillas*) 3))
            ;(print '********************)
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list (obtenerColorEnPosicion (nth 0 *Casillas*) 3))))
            ;(print (string "Movimiento hacia casilla 2"))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion mosaicosEnTabla (list (obtenerColorEnPosicion (nth 0 *Casillas*) 3)) (list 6))))
           
           ((equal 2 (Mosaico-ubicacion mosaicoAux)) ;; checo los vecinos que corresponden a la casilla 3, es a la que me moveré!"
            ;(print (string "Movimiento hacia casilla 3"))
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 2 *Casillas*) 1)
                                                               (obtenerColorEnPosicion (nth 0 *Casillas*) 2) 
                                                               (obtenerColorEnPosicion (nth 1 *Casillas*) 3)
                                                               )))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla 
                                    (list 
                                      (obtenerColorEnPosicion (nth 2 *Casillas*) 1)
                                      (obtenerColorEnPosicion (nth 0 *Casillas*) 2)
                                      (obtenerColorEnPosicion (nth 1 *Casillas*) 3) 
                                      ) 
                                    (list 4 5 6))))
           ((equal 3 (Mosaico-ubicacion mosaicoAux)) ;;checo los vecinos que corresponden a la casilla 4, es a la que me voy a mover!  
            ;(print (string "Movimiento hacia casilla 4"))
            ;(print (Mosaico-ubicacion mosaicoAux))
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 3 *Casillas*) 1) 
                                                               (obtenerColorEnPosicion (nth 1 *Casillas*) 2)
                                                               )))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla 
                                    (list 
                                      (obtenerColorEnPosicion (nth 3 *Casillas*) 1) 
                                      (obtenerColorEnPosicion (nth 1 *Casillas*) 2)
                                      )
                                    (list 4 5)))) ;
           ((equal 4 (Mosaico-ubicacion mosaicoAux)) ;;checo los vecinos que corresponden a la casilla 5, es a la que me voy a mover!  
            ;(print (string "Movimiento hacia casilla 5"))
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 2 *Casillas*) 2) 
                                                               (obtenerColorEnPosicion (nth 3 *Casillas*) 3)
                                                               )))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla 
                                    (list 
                                      (obtenerColorEnPosicion (nth 2 *Casillas*) 2) 
                                      (obtenerColorEnPosicion (nth 3 *Casillas*) 3))
                                    (list 5 6))))
           ((equal 5 (Mosaico-ubicacion mosaicoAux)) ;;checo los vecinos que corresponden a la casilla 6, es a la que me voy a mover!  
            ;(print (string "Movimiento hacia casilla 6"))
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 5 *Casillas*) 1)
                                                               (obtenerColorEnPosicion (nth 3 *Casillas*) 2) 
                                                               (obtenerColorEnPosicion (nth 4 *Casillas*) 3) 
                                                               )))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla 
                                    (list 
                                      (obtenerColorEnPosicion (nth 5 *Casillas*) 1)
                                      (obtenerColorEnPosicion (nth 3 *Casillas*) 2)
                                      (obtenerColorEnPosicion (nth 4 *Casillas*) 3))
                                    (list 4 5 6))))
           ((equal 6 (Mosaico-ubicacion mosaicoAux))
            ;(print (string "Movimiento hacia casilla 7"))
            ;(print *Casillas*)
            (setf mosaicosEnTabla (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 6 *Casillas*) 1)
                                                               (obtenerColorEnPosicion (nth 4 *Casillas*) 2))))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla
                                    (list 
                                     (obtenerColorEnPosicion (nth 6 *Casillas*) 1)
                                     (obtenerColorEnPosicion (nth 4 *Casillas*) 2))
                                    (list 4 5))))
           ((equal 7 (Mosaico-ubicacion mosaicoAux))
            ;(print (string "Movimiento hacia casilla 8"))
            (setf mosaicosValidos (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 5 *Casillas*) 2) 
                                                               (obtenerColorEnPosicion (nth 6 *Casillas*) 3)
                                                               )))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla
                                    (list 
                                     (obtenerColorEnPosicion (nth 5 *Casillas*) 2)
                                     (obtenerColorEnPosicion (nth 6 *Casillas*) 3))
                                    (list 5 6))))
           ((equal 8 (Mosaico-ubicacion mosaicoAux))
            ;(print (string "Movimiento hacia casilla 9"))
            (setf mosaicosValidos (obtenerMosaicosConColores (list 
                                                               (obtenerColorEnPosicion (nth 8 *Casillas*) 1) 
                                                               (obtenerColorEnPosicion (nth 6 *Casillas*) 2) 
                                                               (obtenerColorEnPosicion (nth 7 *Casillas*) 3)
                                                               )))
            (setf mosaicosValidos (rotarMosaicosEnListaConCombinacionHastaPosicion 
                                    mosaicosEnTabla
                                    (list 
                                     (obtenerColorEnPosicion (nth 8 *Casillas*) 1)
                                     (obtenerColorEnPosicion (nth 6 *Casillas*) 2)
                                     (obtenerColorEnPosicion (nth 7 *Casillas*) 3))
                                    (list 4 5 6)))))
         (cond 
           ((equal 9 (Mosaico-ubicacion mosaicoAux));; se llenaron todas las casillas
            (print *Casillas*)
            (read-line)
            (read-line)
            (setf *ClosedList* (sort *ClosedList* #'< :key #'Mosaico-idMosaicoNumerico))
            (rastreaSolucion mosaicoAux)
            (return));;el break de lisp XP
           (T
             (loop for mosaicoValido in mosaicosValidos do
               (cond
                ((and (not (obtenerMosaicoPorLetra (Mosaico-idMosaico mosaicoValido) *openList*)) (not (obtenerMosaicoPorLetra (Mosaico-idMosaico mosaicoValido) *closedList*))) 
                    ;;Se rota el mosaico hasta que coincidan las posiciones, también se añade el ancestroMosaico y la posicion
                    ;(setf mosaicoValido (rotarMosaicoHastaPosicion ))
                    (setf mosaicoAGuardar mosaicoValido)
                    (setf (Mosaico-ubicacion mosaicoAGuardar) (+ (Mosaico-ubicacion mosaicoAux) 1))
                    ;(print mosaicoValido)
                    (setf (Mosaico-ancestroMosaico mosaicoAGuardar) (Mosaico-idMosaicoNumerico mosaicoAux))
                    ;(if
                      ;(not (exiteMosaicoEnCasillas mosaicoAGuardar))
                      ;(setf (nth (Mosaico-ubicacion mosaicoAGuardar) *Casillas*) mosaicoAGuardar))
                    (setf (nth (Mosaico-ubicacion mosaicoAGuardar) *Casillas*) mosaicoAGuardar)
                    ;; Se añade a la primer posición
                    ;(print '---------------------------------------------------------)
                    ;(print '---------------------------------------------------------)
                    ;(print mosaicoAGuardar)
                    ;(print '*********************************************************)
                    ;(print *Casillas*)
                    ;(print '---------------------------------------------------------)
                    ;(print '---------------------------------------------------------)
                    (agregarMosaicoListaAbierta mosaicoAGuardar :modo 'queue)
                    ;(setf *OpenList* (sort *OpenList* #'> :key #'Mosaico-ubicacion))
                    ;(print *OpenList*)
                    ;(read-line)
                    ;(read-line)
                    ;(read-line)
                    ;(read-line)
                    ))))))))))
;; PRUEBAS
(cargaMosaicos nil)
(cargaTablaPosiciones (string "./combinaciones.data"))
;(defun print-hash-entry (key value)
;    (format t "The value associated with the key ~S is ~S type of key ~S~%" key value (type-of key)))
(defvar mosaicoDePrueba  (gethash  '(7 7) *TablaPosiciones*))
;(print mosaicoDePrueba)
;;
;(print  mosaicoDePrueba)
;(print '-----------------------------------------------------)
;(print '-----------------------------------------------------)
;(print '-----------------------------------------------------)
;(print (obtenerTodosLosMovimientos mosaicoDePrueba))
;(print (rotarMosaicosEnListaConCombinacionHastaPosicion mosaicoDePrueba (list 7 7) (list 3 4)))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 1))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 2))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 3))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 4))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 5))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 6))
(print (buscarSoluciones nil))
(print *Casillas*)
;(print (rotarMosaicosEnListaConCombinacionHastaPosicion mosaicoDePrueba (list 7) (list 6)))
;(print '-----------------------------------------------------)
;(print '-----------------------------------------------------)
;(print '-----------------------------------------------------)