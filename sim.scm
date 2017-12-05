;;
;; Human Resource Machine Simulator
;;
;; instruction set
;; - copyfrom addr
;; - copyto addr
;; - sub addr
;; - add addr
;; - bump+ addr
;; - bump- addr
;; - jump label
;; - jump-if-neg label
;; - jump-if-zero label
;; - inbox
;; - outbox

;(define *memory* (make-vector 25 #f))
;(define *accumulator* #f)
;(define *input* ())

(use gauche.record)

(define-record-type hrm-state #t #t
  (memory)
  (accumulator)
  (inputs))

(define (memory-value stat addr) (vector-ref (hrm-state-memory stat) addr))
(define (memory-set! stat addr val) (vector-set! (hrm-state-memory stat) addr val))
(define (indirectize proc) (lambda (stat addr . rest) (apply proc stat (memory-value stat addr) rest)))

(define (hrm-copyfrom! stat addr) (set! (hrm-state-accumulator stat) (memory-value stat addr)))
(define hrm-copyfrom-ind! (indirectize hrm-copyfrom!))

(define (hrm-copyto! stat addr) (memory-set! stat addr (hrm-state-accumulator stat)))
(define hrm-copyto-ind! (indirectize hrm-copyto!))

(define (hrm-sub! stat addr) (set! (hrm-state-accumulator stat) (- (hrm-state-accumulator stat) (memory-value stat addr))))
(define hrm-sub-ind! (indirectize hrm-sub!))

(define (hrm-add! stat addr) (set! (hrm-state-accumulator stat) (+ (hrm-state-accumulator stat) (memory-value stat addr))))
(define hrm-add-ind! (indirectize hrm-add!))

(define (hrm-bump+! stat addr)
  (let ((new-value (+ (memory-value stat addr) 1)))
    (memory-set! stat addr new-value)
    (set! (hrm-state-accumulator stat) new-value)))
(define hrm-bump+-ind! (indirectize hrm-bump+!))

(define (hrm-bump-! stat addr)
  (let ((new-value (- (memory-value stat addr) 1)))
    (memory-set! stat addr new-value)
    (set! (hrm-state-accumulator stat) new-value)))
(define hrm-bump--ind! (indirectize hrm-bump-!))

(define (hrm-inbox! stat)
  (let ((top (car (hrm-state-inputs stat))))
    (set! (hrm-state-accumulator stat) top)
    (set! (hrm-state-inputs stat) (cdr (hrm-state-inputs stat)))))

(define (hrm-outbox! stat)
  (print (hrm-state-accumulator stat))
  (set! (hrm-state-accumulator stat) #f))

(define (hrm-execute! stat prog)
  (let loop ((prog prog))
    (unless (null? prog)
            (let* ((instruction (car prog))
                   (opcode (car instruction)))
              (case opcode
                ((jump) (loop (cadr instruction)))
                ((jump-if-zero) (if (zero? (hrm-state-accumulator stat)) (loop (cadr instruction)) (loop (cdr prog))))
                ((jump-if-neg)  (if (< (hrm-state-accumulator stat) 0)   (loop (cadr instruction)) (loop (cdr prog))))
                (else
                 (begin (apply (car instruction) stat (cdr instruction))
                        (loop (cdr prog)))))))))

;;;;;;
(define (convert-program lis)
  (let loop ((lis lis)
             (dest ()))
    (if (null? lis)
        (reverse dest)
        (let* ((ins (car lis))
               (op (car ins)))
          (cond
           ((symbol? op) (let* ((str (symbol->string op))
                                (first (string-ref str 0)))
                           (if (eq? first #\:) ; label
                               (todo)
                               (let ((proc (eval (string->symbol (string-append "hrm-" str "!")) ())))
                                 (loop (cdr lis) (cons (cons proc (cdr ins)) dest)))
                                 ))))))))

;;;;;;;;;;;

(define program-add
  `((,hrm-inbox!)
    (,hrm-copyto! 0)
    (,hrm-inbox!)
    (,hrm-add! 0)
    (,hrm-outbox!)))

(define program-add-all
  (let* ((init 
         `((,hrm-inbox!)
           (jump-if-zero
            ((,hrm-copyfrom! 0)
             (,hrm-outbox!)))
           (,hrm-add! 0)
           (,hrm-copyto! 0)))
         (loop
          `((jump ,init))
          ))
    (set-cdr! (last-pair init) loop)
    init
    ))

(define stat (make-hrm-state (make-vector 25 #f) #f ()))
(set! (hrm-state-inputs stat) (list 1 2 3 4 5 6 7 8 9 10 0))
(memory-set! stat 0 0)                       ; initialize
(hrm-execute! stat program-add-all)

;;;;;;;;;

(define program-add-converted
  (convert-program
   '((inbox)
     (copyto 0)
     (inbox)
     (add 0)
     (outbox))
   ))

(set! (hrm-state-inputs stat) (list 1 2 3 4 5 6 7 8 9 10 0))
(memory-set! stat 0 0)                       ; initialize
(hrm-execute! stat program-add-converted)

;;;;;;;;;;;;;;;;

#;(define program-multiply
  (convert-program
   `(:start
     (inbox)
     (copyto :a)
     (inbox)
     (copyto :b)

     :loop
     (copyfrom :b)
     (jump-if-zero :end)
     (copyfrom :a)
     (add :sum)
     (bump- :b)
     (jump :loop)

     :end
     (copyfrom :sum)
     (outbox)
     (jump :start)
     )))
