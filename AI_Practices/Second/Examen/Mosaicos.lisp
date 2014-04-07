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
#|
(defun buscarSoluciones (&optional debug)
  (loop for mosaico in *ListaMosaicos* do
    (loop for movimiento in (obtenerTodosLosMovimientos mosaico) do
      
      )))
|#
#|
(defun a_star_search (nodoDestino)
  ;(agregarNodoListaAbierta (crearNodo nil 0 'SinVisitar nil *Entrada* *NivelDelArbol*) :modo'queue);;Se anade el nodo inicial a la frontera de busqueda   
  (agregarMosaicoListaAbierta )
  (let ((nodoAux nil) (movimientosValidos nil) (nodoActual nil) (nodoAnterior nil)) 
    (while (not (null *OpenList*))
      (setq nodoAux (pop *OpenList*)) ;;se obtiene el único elemento de la lista abierta
      (setf (Nodo-estadoNodo nodoAux) 'Visitado) ;;Se cambia el estado del nodo
      (agregarNodoListaCerrada nodoAux);;El nodo que fue expandido se agrega a la lista cerrada   
      (setq movimientosValidos (obtenerMovimientosNodo nodoAux));;se obtienen los movimientos basicos del nodo
      (cond
        ((equal (Nodo-posicionNodo nodoAux) (Nodo-posicionNodo nodoDestino))
         (setf *ClosedList* (sort *ClosedList* #'< :key #'Nodo-idNodo))
         (rastreaSolucion nodoAux)
          (return-from a_star_search *Solucion*)
         )
        (T
          ;;
          (loop for movimiento in movimientosValidos do
            (cond
              ((obtenerNodoPorPosicion (first movimiento) *OpenList*)
               ;;actualizo el nodo si su costo es menor
                (setf nodoAnterior (obtenerNodoPorPosicion (first movimiento) *OpenList* T));;el que se encuentra en la lista abierta actualmente
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
                   (setf *OpenList* (delete-if #'(lambda (x) (equal x nodoAnterior)) *OpenList*))
                   ;;Se agrega el nodo con camino más corto a la lista abierta
                   (agregarNodoListaAbierta nodoActual :modo 'queue)
                   (setf *OpenList* (sort *OpenList* #'< :key #'Nodo-costoNodo))
                   )
                  )
               )
              ((and (not (obtenerNodoPorPosicion (first movimiento) *OpenList*)) (not (obtenerNodoPorPosicion (first movimiento) *ClosedList*))) 
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
                  (setf *OpenList* (sort *OpenList* #'< :key #'Nodo-costoNodo))   
                )
              )
            )
          )
        )
      )
    )
  )
|#