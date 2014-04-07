;;==============================================================================
;;==============================================================================
;; Funcion para cargar la base de datos que contendrá los movimientos
;;==============================================================================
;;==============================================================================
(defvar *TablaPosiciones* (make-hash-table :test 'equal))
(defvar *Casillas* (list 0 0 0 0 0 0 0 0 0))
(defvar *OpenList* nil)
(defvar *ClosedList* nil)
(defvar *Solucion* nil)
(defvar *ContadorSoluciones* 0)
(defvar *ContadorNodo* 0)
(defvar *ListaMosaicos* nil)
(load "Mosaicos.lisp")
(defun limpiarListas (debug)
  (setf *Casillas* (list 0 0 0 0 0 0 0 0 0))
  (setf *OpenList* nil)
  (setf *ClosedList* nil)
  (setf *Solucion* nil)
  )
(defun cargaMosaicos (debug)
  (setf *ListaMosaicos* (cons (make-mosaico 
                                :idMosaico 'A
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 1 6 4 5 2) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'B 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(4 3 10 2 1 1) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'C 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(2 4 4 6 6 9) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'D 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(7 6 2 4 3 5) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'E
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0 
                                :colores '(1 4 4 9 2 2) 
                                :gradosDeRotacion 0 
                                :ubicacion '( 0 0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'F 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(4 7 1 1 6 4) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'G 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(6 3 9 8 2 1) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'H 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(10 7 8 7 2 1) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'I 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(10 9 2 10 3 5) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'J 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(2 9 4 4 10 5) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'K 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 9 9 3 4 6) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'L 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(5 10 6 7 6 4) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'M 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(4 3 4 6 7 7) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'N 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 1 6 3 1 8) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'O 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(5 1 7 6 3 4) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'P 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(7 2 9 2 5 8) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'Q 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 6 7 4 1 2) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'R 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(1 1 6 2 1 4) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'S 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(3 3 1 4 6 8) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  (setf *ListaMosaicos* (cons (make-Mosaico 
                                :idMosaico 'T 
                                :ancestroMosaico nil
                                :costoMosaico 0
                                :numeroDeRotaciones 0
                                :colores '(9 4 7 7 1 8) 
                                :gradosDeRotacion 0 
                                :ubicacion '(0)) 
                              *ListaMosaicos*))
  )
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
(defun obtenerMosaicosConColores (colores)
  (return-from obtenerMosaicosConColores (gethash colores *TablaPosiciones*)))
(defun rotarMosaicoHastaPosicion (mosaico elementoEnMosaico posicionFinal)
  (loop until (equal (first elementoEnMosaico) (nth (- posicionFinal 1) (Mosaico-colores mosaico))) do
    (setf (Mosaico-colores mosaico) (append (last (Mosaico-colores mosaico)) (butlast (Mosaico-colores mosaico))))
    ;:numeroDeRotaciones 0
    (incf (Mosaico-numeroDeRotaciones mosaico) 1)
    (incf (Mosaico-gradosDeRotacion mosaico) 30)
    )
  mosaico)
(defun obtenerTodosLosMovimientos (mosaico)
  (let ((listaMovimientos nil))
    (loop for i from 1 below 7 do
      (print i)
      (print (rotarMosaicoHastaPosicion mosaico (list (first (Mosaico-colores mosaico))) i)) )
    listaMovimientos))
;; PRUEBAS
(cargaMosaicos nil)
(cargaTablaPosiciones (string "./combinaciones.data"))
(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S type of key ~S~%" key value (type-of key)))
(defvar mosaicoDePrueba (first (gethash  '(6 4) *TablaPosiciones*)))
(print mosaicoDePrueba)
;;
(print '-----------------------------------------------------)
(print '-----------------------------------------------------)
(print '-----------------------------------------------------)
(print (obtenerTodosLosMovimientos mosaicoDePrueba))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 1))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 2))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 3))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 4))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 5))
;(print (rotarMosaicoHastaPosicion mosaicoDePrueba (list 7) 6))
(print '-----------------------------------------------------)
(print '-----------------------------------------------------)
(print '-----------------------------------------------------)