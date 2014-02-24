;Reséndiz Arteaga Juan Alberto
;Tarea 1
;Funcion para obtener el elemento en una posición determinada
(setf *random-state* (make-random-state t))
(defun elementoEnPosicion (elemento lista position) 
  
    (cond 
      ((null lista) nil);lista vacia
      ((< position 0) nil);posicion negativa
      (
        (equal position 0) ;induccion de la llamada iterativa
        (if (equal (first lista) elemento) T nil) 
      )
      (
        T
        (print lista) 
        (elementoEnPosicion elemento (rest lista) (- position 1)) ;llamada recursiva
      )
    )
  
)
;Prueba de escritorio
;(print (elementoEnPosicion 'a '(b b b b a) 4))
;-----------------------------------------------------------------------
;Funcion para regresar el primer impar de una lista
(defun primerImpar (lista)
  (cond
    ((null lista) nil)
    ((equal (mod (first lista) 2) 1) (first lista) )
    (T (primerImpar (rest lista))) 
  )
)
;Prueba de escritorio
;(print (primerImpar '(2 2 3 4 5 6)))
;-----------------------------------------------------------------------
;Funcion para regresar el ultimo elemento de una lista que sea mayor o igual a cero
(defun ultimoElementoMayorIgualCero (lista)
  (cond
    ((null lista) nil)
    (
      (numberp (first lista))
      (
        if(>= (first lista) 0)
        (last (cons (first lista) (ultimoElementoMayorIgualCero (rest lista))))
        )
      )
    (T (ultimoElementoMayorIgualCero (rest lista)))
    )
  )
;Prueba de escritorio
;(print (ultimoElementoMayorIgualCero '(1 2 3 4 4 5 a u 9)))
;-----------------------------------------------------------------------
;Funcion para aplanar una lista con más listas dentro
(defun aplanarListas (lista)
  (cond 
    ((null lista) nil)
    ((atom (first lista))
        (cons (first lista) (aplanarListas (rest lista))))
    (T
      (append (aplanarListas (first lista)) (aplanarListas (rest lista)))
      )
  )
 )
;Prueba de escritorio
;(print (aplanarListas '(a (b c d) e f (g h))))
;-----------------------------------------------------------------------
;Funcion para ordenar una lista de numeros usando el metodo de la burbuja
(defun ordenamientoBurbuja (lista) 
  (cond 
    ((null lista) nil)
    ((<= (length lista) 1) lista)
    (T
      (let ((aux 0))
        (loop for i from 0 below (length lista) do 
          (loop for j from (+ i 1) below (length lista) do
            (cond 
              ((< (nth j lista) (nth i lista))
                (setf aux (nth j lista))
                (setf (nth j lista) (nth i lista))
                (setf (nth i lista) aux)
                )
              )
            )
          ) 
        )
      lista
      )
    )
  )
;Prueba de escritorio
;(print (ordenamientoBurbuja '(1 3 2 5 4)))
;-----------------------------------------------------------------------
;Funcion para ordenar una lista de numeros usando el metodo recursivo quicksort
;Se cambia un elemento dentro de la lista de manera aleatoria para mejorar el tiempo de respuesta
(defun quickSortRandomize (lista)
  (let ((aux 0) (indice 0))
    (setf indice (random (length lista))) 
    (setf aux (nth indice lista))
	(setf (nth indice lista) (nth 0 lista))
	(setf (nth 0 lista) aux)
    )
  (print lista)
  (quickSort lista)
  )
(defun quickSort (lista)
  (let ((less ()) (pivot -1) (grt ()) (listadoFinal ()) )
  (cond
    ((<= (length lista ) 1) lista)
    (T
        (setf pivot (+ (nth (- (length lista) 1) lista) 0))        
        (loop for i from 0 below  (- (length lista) 1) do
          ;(print (type-of (nth i lista)))
          (if (< (nth i lista) pivot)
            (setf less (append (list (nth i lista)) less))
            (setf grt (append (list (nth i lista)) grt))
            )
          )
      (append (quickSort less) (list pivot) (quicksort grt))
        )
      )
    )
  )
;(print (quickSortRandomize '(10 20 30 40 50 60 70 80 90 70)))
;-----------------------------------------------------------------------
;Funcion para obtener la cantidad de elementos de una lista, sea esta anidada o no
(defun elementosTotales (lista)
  (cond 
    ((null lista) 0)
    ((atom (first lista))
        (+ 1 (elementosTotales (rest lista))))
    (T
      (+ (elementosTotales (first lista)) (elementosTotales (rest lista)))
      )
  )
 )
;Prueba de Escritorio
;(print (elementosTotales '(a (b c d) e f (g h))))
;-----------------------------------------------------------------------
;Funcion para la diagonal de una matriz cuadrada
(defun diagonal (matriz)
  (let ((lista ()))
     (loop for x from 0 below (first (array-dimensions matriz)) do
        (setf lista (append (list (aref matriz x x)) lista))
        )
    lista
    )
  )
;Prueba de escritorio
;(defvar mat-cuadrada (make-array '(3 3) :initial-contents '((1 2 3)(4 5 6)(7 8 9))))
;(print (diagonal mat-cuadrada))
;-----------------------------------------------------------------------
;Funcion similar a tic tac toe
(defun ticTacToe (matriz pos caracter)
  (cond
    ((/= (first (array-dimensions matriz)) (second (array-dimensions matriz))) nil)
    (T
      (let ((i 0) (j 0) (contador 1 ) (bandera nil))
         (loop until (equal bandera T) do
           (cond 
             ((= contador pos)
              (setf (aref matriz i j) caracter)
              (setf bandera T))
             ((= i 3)
               (setf i 0)
               (setf j (+ j 1))
               )
             ((= j 3)
               (setf j 0)
               (setf i (+ i 1))
             )
             (T
               (setf i (+ i 1))
               ))
           (setf contador (+ contador 1))
           )
        )
      matriz      
      )
    )
  )
;Prueba de escritorio
;(defvar mat-cuadrada (make-array '(3 3)))
;(print (ticTacToe mat-cuadrada 9 #\x))
;-----------------------------------------------------------------------
;Funcion que regresa una lista 