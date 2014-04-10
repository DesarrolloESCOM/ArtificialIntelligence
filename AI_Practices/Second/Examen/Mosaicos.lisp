;;==============================================================================
;;==============================================================================
;; Estructura que define al Mosaico, junto con los métodos de utilidad para éste.
;;==============================================================================
;;==============================================================================
(defstruct
  Mosaico
;;
  idMosaico
  ancestroMosaico
  costoMosaico
  numeroDeRotaciones
  colores
  gradosDeRotacion
  ubicacion)
;===============================================================================
;(defvar *ordenCasillas* (list 3 0 7 4 1 8 5 2 9 6))
(defvar *Casillas* (list 0 0 0 0 0 0 0 0 0))
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
(defun obtenerMosaico (idMosaico lista &optional (regresaEstructura nil))
  (let ((nodoEncontrado nil))
    (loop for mosaico in lista do
           (cond 
                ((equal (Mosaico-idMosaico mosaico) idMosaico)
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
(defun rastreaSolucion (mosaicoARastrear)
    (setf *Solucion* (append *Solucion* (list (list (Mosaico-idMosaico mosaicoARastrear) (Mosaico-gradosDeRotacion mosaicoARastrear)))))
    (let ((bandera T) (nodoAux nil))
      (setq nodoAux (obtenerMosaico *ClosedList* (Mosaico-ancestroMosaico mosaicoARastrear)))
      (while 
        (not (equal (Mosaico-ancestroMosaico nodoAux) nil))
          (setf *Solucion* (append *Solucion* (list (list (Mosaico-idMosaico nodoAux) (Mosaico-gradosDeRotacion nodoAux)))))    
          (setf nodoAux (obtenerMosaico *ClosedList* (Mosaico-ancestroMosaico nodoAux))))
      (print '-----------------------------------------------------------------------)
      (print *Solucion*)
      (limpiarListas nil)
      (incf *ContadorSoluciones* 1)))