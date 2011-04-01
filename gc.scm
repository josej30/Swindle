;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clase memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <memory> ()
  (cells :type <vector>
         :accessor cells
         :initarg :cells
         :documentation "Celdas de la memoria"
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clase cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <cell> ()
  (crap :type <boolean>
        :accessor crap?
        :initvalue true
        :documentation "La celda contiene basura?"
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clase valor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <val> (<cell>)
  (value :accessor value
         :initarg :value
         :documentation "El valor de una celda"
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clase manejador de memoria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <managed-memory> (<memory>)
  (freelist :type <list>
            :accessor freelist
            :initarg :freelist
            :documentation "Lista de bloques de memoria física disponible"
            )
  (roots :type <list>
         :accessor roots
         :initarg :roots
         :documentation "Lista de objetos que se encuentran vivos en memoria"
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clase Pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <ptr> ()
  (points-to :type <number>
             :accessor points-to
             :initarg :points-to
             :documentation "Direccion de memoria a la que 
                             referencia el apuntador"
             )
  (is-null :type <boolean> 
           :accessor is-null?
           :initarg :is-null
           :documentation "Informa si el apuntador es nulo o no"
           )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clase cons-cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <cons> (<cell>)
  (car :type <ptr>
       :accessor head
       :initarg :head
       :documentation "Apuntador a la cabeza del cons"
       )
  (cdr :type <ptr>
       :accessor tail
       :initarg :tail
       :documentation "Apuntador a la cola del cons"
       )
  )


;;;;;;;;;;;;;;;; Metodos de memoria ;;;;;;;;;;;;;;;;;;;;;

;;
;; Método cons-memoria
;; 
;; Recibe un entero n y devuelve una memoria de tamaño n
(defmethod cons-memoria ((n <number>))
  (if (< n 1)
      (printf "memory must have at least one cell")
      (make <memory> :cells (make-vector n (cons-cell))))
  )

;;
;; Método size
;; 
;; Recibe una memoria y devuelve el tamaño de la misma
(defmethod size ((mem <memory>))
  (vector-length (cells mem))
  )

;;
;; Método fetch
;; 
;; Recibe una memoria mem y un numero n. Devuelve el elemento
;; almacenado en la n-ésima posición de la memoria mem.
(defmethod fetch ((mem <memory>) (n <number>))
  (if (or (< n 0) (>= n (size mem)))
      (printf "bus error -- core dumped")
      (vector-ref (cells mem) n)
      )
  )

;;
;; Método store!
;; 
;; Recibe una memoria mem, un numero n y una celda x. 
;; Devuelve la memoria mem con la celda x almacenada en 
;; la posición n de la misma
(defmethod store! ((mem <memory>) (n <number>) (x <cell>))
  (if (or (< n 0) (>= n (size mem)))
      (printf "bus error -- core dumped")
      (vector-set! (cells mem) n x)
      )
  )


;;;;;;;;;;;;;;;; Metodos de cell ;;;;;;;;;;;;;;;;;;;;;

;;
;; Método crap!
;; 
;; Recibe una celda c y un booleando b.
;; Devuelve la celda con el slot crap cambiado con el valor b
(defmethod crap! ((c <cell>) (b <boolean>))
  (set! (crap? c) b)
  )

;;
;; Método cons-cell!
;; 
;; Constructor de celdas. No recibe parámetros y devuelve una
;; celda nueva.
(defmethod cons-cell ()
  (make <cell>)
  )


;;;;;;;;;;;;;;;; Metodos del manejador de memoria ;;;;;;;;;;;;;;;;;;

;;
;; Método cons-manejador-memoria
;; 
;; Constructor de un manejador de memoria. Recibe un número n y devuelve
;; un manejador de memoria con una memoria de tamaño n.
(defmethod cons-manejador-memoria ((n <number>))
  (if (< n 1)
      (printf "memory must have at least one cell")
      (make <managed-memory> :cells (make-vector n (cons-cell)) 
            :freelist (build-list n values) :roots (list) )
      )
  )

;;
;; Método store-object!
;; 
;; Recibe un manejador de memoria, un símbolo (nombre) y un valor.
;; Devuelve el manejador de memoria modificado con el valor. Esto es:
;;
;; 1) Modificar el slot "roots" del manejador de memoria para identificar
;; la posición donde se está guardando el objeto.
;; 2) Modificar el slot "freelist" para reflejar el nuevo estado de las
;; celdas que se encuentran libres en la memoria.
;; 3) Modificar el slot "cells" de la memoria para almacenar las celdas que
;; hagan falta para representar el nuevo objeto.
(defmethod store-object! ((mm <managed-memory>) (nombre <symbol>) (valor))
  ( cond 
     ((list? valor) (begin
                      (cond ((checkSize mm (medir valor))
                            (let 
                                ((dir (storeList mm nombre valor 'cabeza)))
                              ( begin
                                 (set! (roots mm) (cons (list nombre dir) (roots mm)))
                                 dir
                                 )
                              ))
                      (else
                       (printf "No hay memoria")
                       )
                      )
                    ))
     
     (else (begin 
             (cond ((checkSize mm 1) (storeVal mm nombre valor))
                   (else
                    (printf "No hay memoria")
                    )
                   )
             )
           )
     )
  )

;;
;; Método storeVal
;; 
;; Recibe un manejador de memoria, un símbolo (nombre) y un valor.
;; Devuelve el manejador de memoria modificado con el valor. Esta
;; función es específica para cuando el argumento "valor" NO es una lista.
(defmethod storeVal ((mm <managed-memory>) (nombre <symbol>) (valor))
  (let ((pos (car (freelist mm))))
    (begin
     (set! (roots mm) (cons (list nombre pos) (roots mm)))
     (set! (freelist mm) (cdr (freelist mm)))
     (vector-set! (cells mm) pos (make <val> :value valor))
     pos
     )
    )
  )

;;
;; Método storeList
;; 
;; Recibe un manejador de memoria, un símbolo (nombre) y un valor.
;; Devuelve el manejador de memoria modificado con el valor. Esta
;; función es específica para cuando el argumento "valor" es una lista.
;; Por lo tanto es una función recursiva que se va llamando a sí misma
;; hasta que termina de almacenar correctamente todos los elementos 
;; necesarios de la lista.
(defmethod storeList ((mm <managed-memory>) (nombre <symbol>) (valor) 
                                            (id <symbol>))
     
  ( cond
     
     ;; Caso en el que la lista sea de tamaño 1
     ((eq? (length valor) 1) (let (
                                    (dirCons (car (freelist mm)))
                                    (dirVal (car (cdr (freelist mm))))
                                    (head (car valor))
                                    )
                                ( begin
                                   (set! (freelist mm) (cdr (freelist mm)))
                                   (set! (freelist mm) (cdr (freelist mm)))
                                   (vector-set! (cells mm) dirVal (make <val> :value head))
                                   (vector-set! (cells mm) dirCons (make <cons> :head (make <ptr> :points-to dirVal :is-null #f) :tail (make <ptr> :points-to -1 :is-null #t)))
                                   dirCons
                                   )                    
       ))
     
     ;; Caso en el que la lista sea de tamaño 2
     ((eq? (length valor) 2) (let (
                                    (dirCons (car (freelist mm)))
                                    (dirVal (car (cdr (freelist mm))))
                                    (dirUltCons (car (cdr (cdr (freelist mm)))))
                                    (dirUltVal (car (cdr (cdr (cdr (freelist mm))))))
                                    (head (car valor))
                                    (ultHead (car (cdr valor)))
                                    )
                                ( begin
                                   (set! (freelist mm) (cdr (freelist mm)))
                                   (set! (freelist mm) (cdr (freelist mm)))
                                   (set! (freelist mm) (cdr (freelist mm)))
                                   (set! (freelist mm) (cdr (freelist mm)))
                                   (vector-set! (cells mm) dirVal (make <val> :value head))
                                   (vector-set! (cells mm) dirUltVal (make <val> :value ultHead))
                                   (vector-set! (cells mm) dirCons (make <cons> :head (make <ptr> :points-to dirVal :is-null #f) :tail (make <ptr> :points-to dirUltCons :is-null #f)))
                                   (vector-set! (cells mm) dirUltCons (make <cons> :head (make <ptr> :points-to dirUltVal :is-null #f) :tail (make <ptr> :points-to -1 :is-null #t)))
                                   dirCons
                                   )              
       ))
     
     ;; Caso en el que la lista sea de tamaño mayor que 2
     ( else
       
       (let (
             (pos1 (car (freelist mm)))
             (head (car valor))
             (valor (cdr valor))
             )
         ( begin 
            (set! (freelist mm) (cdr (freelist mm)))
            ( cond 
               ((eq? id 'cabeza) ( begin 
                                    (let (
                                          (pos2 (car (freelist mm)))
                                          )
                                      (set! (freelist mm) (cdr (freelist mm)))
                                      (vector-set! (cells mm) pos2 (make <val> :value head))
                                      ;;(printf "Guardo ~a en ~a\n" head pos2)
                                      (let (
                                            (pos3 (storeList mm nombre valor 'cuerpo))
                                            )
                                        (vector-set! (cells mm) pos1 (make <cons> :head (make <ptr> :points-to pos2 :is-null #f) :tail (make <ptr> :points-to pos3 :is-null #f)))
                                        )
                                      pos1
                                      )
                                    )
                                 )
               ((null? (cdr valor)) ( begin
                                       (let (
                                             (pos2 (car (freelist mm)))
                                             (ultValor (car valor))
                                             (dirultApuntador (car (cdr (cdr (freelist mm)))))
                                             (dirUltValor (car (cdr (freelist mm))))
                                             )
                                         (set! (freelist mm) (cdr (freelist mm)))
                                         (set! (freelist mm) (cdr (freelist mm)))
                                         (set! (freelist mm) (cdr (freelist mm)))
                                         (vector-set! (cells mm) pos1 (make <cons> :head (make <ptr> :points-to pos2 :is-null #f) :tail (make <ptr> :points-to dirultApuntador :is-null #f)))
                                         ;;(printf "Guardo el penul apuntador en ~a\n" pos1)
                                         (vector-set! (cells mm) pos2 (make <val> :value head))
                                         ;;(printf "Guardo ~a en ~a\n" head pos2)
                                         (vector-set! (cells mm) dirultApuntador (make <cons> :head (make <ptr> :points-to dirUltValor :is-null #f) :tail (make <ptr> :points-to -1 :is-null #t)))
                                         (vector-set! (cells mm) dirUltValor (make <val> :value ultValor))
                                         ;;(printf "Guardo ~a en ~a\n" ultValor dirUltValor)
                                         pos1
                                         )
                                       )
                                    )
               (else ( begin 
                        (let (
                              (pos2 (car (freelist mm)))
                              )
                          (set! (freelist mm) (cdr (freelist mm)))
                          (vector-set! (cells mm) pos2 (make <val> :value head))
                          ;;(printf "Guardo ~a en ~a\n" head pos2)
                          (let (
                                (pos3 (storeList mm nombre valor 'cuerpo))
                                )
                            (vector-set! (cells mm) pos1 (make <cons> :head (make <ptr> :points-to pos2 :is-null #f) :tail (make <ptr> :points-to pos3 :is-null #f)))
                            )
                          pos1
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
;; Método checkSize
;; 
;; Recibe un manejador de memoria mm y un numero n. Retorna
;; #t si hay n cantidad de memoria disponible en el manejador
;; y #f en caso contrario.
(defmethod checkSize ((mm <managed-memory>) (n <number>))
  ( cond
     ((< (length (freelist mm) ) n ) #f)
     (else #t)
     )
  )

;;
;; Mutator de is-null
;; Se encarga de asignar un nuevo valor b, a la propiedad is-null de p
(defmethod is-null! ((p <ptr>) (b <boolean>))
  (set! (is-null? p) b)
  )

;;
;; Metodo medir
;; Se encarga de verificar el tamaño de una lista, para ver si la lista 
;; ingreseada cabe en la memoria o no
(defmethod medir ((x <list>))
  (if (null? x) 
      0
      (if (or (symbol? (car x)) (number? (car x)))
	  (+ 2 (medir (cdr x)))
	  (+ 1 (+ (medir (car x)) (medir (cdr x))))
	  )
      )
  )

;;
;; Fecth object
;; Se encarga de mostrar al elemento s, si está definido en mm, en caso de que 
;; no esté definido s en mm muestra el mensaje de 'undefyned symbol'
(defmethod fetch-object ((mm <managed-memory>) ( s <symbol>))
  (let ((l (pertenece (roots mm) s)))
    (if (not (negative? l)) (crearLista mm l)
	(printf "undefyned symbol")
	)
    )
  )

;;
;; Forget object
;; Se elimina al elemento s de la lista de memoria de definiciones de mm
(defmethod forget-object! ((mm <managed-memory>) ( s <symbol>))
  (let ((l (pertenece (roots mm) s)))
    (if (not (negative? l)) (delete (roots mm) s)
	(printf "undefyned symbol")
	)
    )
  )

;;
;; delete (Aux de delete)
;; Se encarga de eliminar al elemento s de la lista l
(defmethod delete ((l <list>) (s <symbol>))
  (if (null? l) '()
      (if (eq? (car (car (roots m))) s) (delete (cdr  l) s)
	  (cons (car (car l)) (delete (cdr l) s))
	  )
      )
  )

;;
;; pertenece (Aux de Fetch y de Forget)
;; Se encarga de verificar si el elemento s, se encuentra en la lista s
;; en caso de estar s, se muestra la primera dirección del mismo
;; en caso de que no esté, devuelve -1
(defmethod pertenece ((l <list>) (s <symbol>))
  (if (null? l) -1
      (if (eq? (car (car l)) s) (car (cdr (car l)))
	  (pertenece (cdr l) s)
	  )
      )
  )

;;
;; crearLista (Aux de Fetch)
;; Se encarga de recorrer toda la memoria m desde la dirección inicial n,
;; y así crear el valor del símbolo almacenado en n, a pesar de ser una
;; lista, valor o símbolos 
(defmethod crearLista ((m <memory>) (n <number>))  
  (let ((e (fetch m n)))
    (if (eq? (class-of e) <val>) (value e)
	(if (is-null? (head e)) '() 
	    (if (is-null? (tail e)) (cons (crearLista m (points-to (head e))) '())
		(cons (crearLista m (points-to (head e))) (crearLista m (points-to (tail e))))
		)
	    )
	)
    )
  )

