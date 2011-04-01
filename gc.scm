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
  (value :type <symbol>
         :accessor value
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

;;;;;;;;;;;;;;;; Metodos de memoria ;;;;;;;;;;;;;;;;;;;;;

;; Constructor de memoria
(defmethod cons-memoria ((n <number>))
  (if (< n 1)
      (printf "memory must have at least one cell")
      (make <memory> :cells (make-vector n (cons-cell))))
  )

;; Tamaño de la memoria
(defmethod size ((mem <memory>))
  (vector-length (cells mem))
  )

;; Fetch del n-esimo elemento de la memoria
(defmethod fetch ((mem <memory>) (n <number>))
  (if (or (< n 0) (>= n (size mem)))
      (printf "bus error -- core dumped")
      (vector-ref (cells mem) n)
      )
  )

;; Cambia el n-esimo valor de la memoria por x
(defmethod store! ((mem <memory>) (n <number>) (x <cell>))
  (if (or (< n 0) (>= n (size mem)))
      (printf "bus error -- core dumped")
      (vector-set! (cells mem) n x)
      )
  )

;;;;;;;;;;;;;;;; Metodos de cell ;;;;;;;;;;;;;;;;;;;;;

;; Mutator de Crap
(defmethod crap! ((c <cell>) (b <boolean>))
  (set! (crap? c) b)
  )

;; Constructor de cell
(defmethod cons-cell ()
  (make <cell>)
  )


;;;;;;;;;;;;;;;; Metodos de valor ;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;; Metodos del manejador de memoria ;;;;;;;;;;;;;;;;;;;;;

;; Constructor de memoria
(defmethod cons-manejador-memoria ((n <number>))
  (if (< n 1)
      (printf "memory must have at least one cell")
      (make <managed-memory> :cells (make-vector n (cons-cell)) :freelist (build-list n values) :roots (list) )
      )
  )

(defmethod store-object! ((mm <managed-memory>) (nombre <symbol>) (valor))
  ( cond 
      ((checkSize mm 1) 
       ( cond 
          ((list? valor) (storeList mm nombre valor))
          (else (storeVal mm nombre valor))
          )
       )
      ( else 
        (printf "No hay memoria")
       )
      )
  )

;; Para guardar valores
(defmethod storeVal ((mm <managed-memory>) (nombre <symbol>) (valor))
  (let ((pos (car (freelist mm))))
    (begin
     (set! (roots mm) (cons (list nombre pos) (roots mm)))
     (set! (freelist mm) (cdr (freelist mm)))
     pos
     )
    )
  )

;; Para guardar listas
(defmethod storeList ((mm <managed-memory>) (nombre <symbol>) (valor))
  (let (
        (pos (car (freelist mm)))
        (head (car valor))
        )
    (begin
     (set! (roots mm) (cons (list nombre pos) (roots mm)))
     (set! (freelist mm) (cdr (freelist mm)))
     pos
     )
    )
  )

(defmethod checkSize ((mm <managed-memory>) (n <number>)) 
  ( cond
     ((< (length (freelist mm) ) n ) #f)
     (else #t)
     )
  )

;; (define m (cons-memoria 10))
;; (fetch m 4)
;; (store! m 5 90)
;; (define c (cons-cell))
;; (crap! c true)
;; (define m (cons-manejador-memoria 10)) (store-object! m 'foo '(1 2 3))
;; (define m (cons-manejador-memoria 10)) (store-object! m 'foo 'bar)
;; (checkSize m 6)


;; Lo que pegue yo! por si acaso da error! :)

;; Clase Pointer
(defclass <ptr> ()
  (points-to :type <number>
             :accessor points-to
             :initarg :points-to
             :documentation "Direccion de memoria a la que referencia el apuntador"
             )
  (is-null :type <boolean> 
           :accessor is-null?
           :initarg :is-null
           :documentation "Informa si el apuntador es nulo o no"
           )
  )

;; Mutator de is-null
(defmethod is-null! ((p <ptr>) (b <boolean>))
  (set! (is-null? p) b)
  )

(defmethod medir ((x <list>))
  (if (null? x) 
      0
      (if (or (symbol? (car x)) (number? (car x)))
	  (+ 2 (medir (cdr x)))
	  (+ 1 (+ (medir (car x)) (medir (cdr x))))
	  )
      )
  )

;; Clase Cons-Cell
(defclass <cons> (<cell>)
  (car :type <ptr>
       :accessor head
       :initarg :head
       :documentation "Alo"
       )
  (cdr :type <ptr>
       :accessor tail
       :initarg :tail
       :documentation "Alo"
       )
  )

;;;;;;;;;;;;;; Prueba de Lezy  ;;;;;;;;;;;;;;;;;;;;
;; define x (make <ptr> :point 

;;;;;;;;;;;;;; Lista de Vaina q quiere Striko ;;;;;;;;;;;
;; (define x (make <ptr> :points-to '5 :is-null #f))
;; (define y (make <ptr> :points-to '4 :is-null #f))
;; (define x (make <ptr> :points-to '5 :is-null #f))
;; (define y (make <ptr> :points-to '3 :is-null #f))
;; (define c (make <cons> :car x :cdr y))
;; (define x (cons-memoria 5))