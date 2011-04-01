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
             :documentation "Direccion de memoria a la que referencia el apuntador"
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
       :documentation "Alo"
       )
  (cdr :type <ptr>
       :accessor tail
       :initarg :tail
       :documentation "Alo"
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

;; (define m (cons-manejador-memoria 100)) (store-object! m 'foo '(1 2 3 4 5 6))

(defmethod store-object! ((mm <managed-memory>) (nombre <symbol>) (valor))
  ( cond 
     ((checkSize mm (medir valor)) 
      ( cond 
         
         ((list? valor) (let 
                            ((dir (storeList mm nombre valor 'cabeza)))
                          ( begin
                             (set! (roots mm) (cons (list nombre dir) (roots mm)))
                             dir
                             )
                          ))
         (else (storeVal mm nombre valor))
         )
      )
      (else 
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
     (vector-set! (cells mm) pos (make <val> :value valor))
     pos
     )
    )
  )

;; Para guardar listas
(defmethod storeList ((mm <managed-memory>) (nombre <symbol>) (valor) (id <symbol>))
     
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


(defmethod checkSize ((mm <managed-memory>) (n <number>)) 
  ( cond
     ((< (length (freelist mm) ) n ) #f)
     (else #t)
     )
  )

;; corrida lezi
;; (define m (cons-manejador-memoria 100))
;; (store-object! m 'x '(1 2 foo 3))
;; (is-null? (head (fetch m 0)))

;; (define m (cons-memoria 10))
;; (fetch m 4)
;; (store! m 5 90)
;; (define c (cons-cell))
;; (crap! c true)
;; (define m (cons-manejador-memoria 100)) (store-object! m 'foo '(1 2 3 4 5 6))
;; (define m (cons-manejador-memoria 100)) (store-object! m 'foo '(1 2 foo))
;; (define m (cons-manejador-memoria 100)) (store-object! m 'foo '(1 2))
;; (define m (cons-manejador-memoria 10)) (store-object! m 'foo 'bar)
;; (checkSize m 6)
;; (points-to (head (vector-ref (cells m) 4)))


;; Lo que pegue yo! por si acaso da error! :)

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

;;;;;;;;;;;;;; Prueba de Lezy  ;;;;;;;;;;;;;;;;;;;;
;; define x (make <ptr> :point 

;;;;;;;;;;;;;; Lista de Vaina q quiere Striko ;;;;;;;;;;;
;; (define x (make <ptr> :points-to '5 :is-null #f))
;; (define y (make <ptr> :points-to '4 :is-null #f))
;; (define x (make <ptr> :points-to '5 :is-null #f))
;; (define y (make <ptr> :points-to '3 :is-null #f))
;; (define c (make <cons> :car x :cdr y))
;; (define x (cons-memoria 5))


;; Mutator de is-null
(defmethod is-null! ((p <ptr>) (b <boolean>))
  (set! (is-null? p) b)
  )

;; Fecth object Mode
(defmethod fetch-object ((mm <managed-memory>) ( s <symbol>))
  (let ((l (pertenece (roots mm) s)))
    (if (not (negative? l)) (crearLista mm l)
	(printf "undefyned symbol")
	)
    )
  )

;; Forget
(defmethod forget-object! ((mm <managed-memory>) ( s <symbol>))
  (let ((l (pertenece (roots mm) s)))
    (if (not (negative? l)) (delete (roots mm) s)
	(printf "undefyned symbol")
	)
    )
  )

(defmethod delete ((l <list>) (s <symbol>))
  (if (null? l) '()
      (if (eq? (car (car (roots m))) s) (delete (cdr  l) s)
	  (cons (car (car l)) (delete (cdr l) s))
	  )
      )
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

(defmethod pertenece ((l <list>) (s <symbol>))
  (if (null? l) -1
      (if (eq? (car (car l)) s) (car (cdr (car l)))
	  (pertenece (cdr l) s)
	  )
      )
  )

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

