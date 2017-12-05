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

(define *memory* (make-vector 25 #f))
(define *accumulator* #f)
(define *input* ())

(define (memory-value addr) (vector-ref *memory* addr))
(define (memory-set! addr val) (vector-set! *memory* addr val))
(define (indirectize proc) (lambda (addr . rest) (apply proc (memory-value addr) rest)))

(define (hrm-copyfrom! addr) (set! *accumulator* (memory-value addr)))
(define hrm-copyfrom-ind! (indirectize hrm-copyfrom!))

(define (hrm-copyto! addr) (memory-set! addr *accumulator*))
(define hrm-copyto-ind! (indirectize hrm-copyto!))

(define (hrm-sub! addr) (set! *accumulator* (- *accumulator* (memory-value addr))))
(define hrm-sub-ind! (indirectize hrm-sub!))

(define (hrm-add! addr) (set! *accumulator* (+ *accumulator* (memory-value addr))))
(define hrm-add-ind! (indirectize hrm-add!))

(define (hrm-bump+! addr)
  (let ((new-value (+ (memory-value addr) 1)))
    (memory-set! addr new-value)
    (set! *accumulator* new-value)))
(define hrm-bump+-ind! (indirectize hrm-bump+!))

(define (hrm-bump-! addr)
  (let ((new-value (- (memory-value addr) 1)))
    (memory-set! addr new-value)
    (set! *accumulator* new-value)))
(define hrm-bump--ind! (indirectize hrm-bump-!))

(define (hrm-inbox!)
  (let ((top (car *input*)))
    (set! *accumulator* top)
    (set! *input* (cdr *input*))))

(define (hrm-outbox!)
  (print *accumulator*)
  (set! *accumulator* #f))

(define (hrm-execute! prog)
  (let loop ((prog prog))
    (unless (null? prog)
            (let* ((instruction (car prog))
                   (opcode (car instruction)))
              (case opcode
                ((jump) (loop (cadr instruction)))
                ((jump-if-zero) (if (zero? *accumulator*) (loop (cadr instruction)) (loop (cdr prog))))
                ((jump-if-neg)  (if (< *accumulator* 0)   (loop (cadr instruction)) (loop (cdr prog))))
                (else
                 (begin (apply (car instruction) (cdr instruction))
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

(set! *input* (list 1 2 3 4 5 6 7 8 9 10 0))
(memory-set! 0 0)                       ; initialize
(hrm-execute! program-add-all)

;;;;;;;;;

(define program-add-converted
  (convert-program
   '((inbox)
     (copyto 0)
     (inbox)
     (add 0)
     (outbox))
   ))

(set! *input* (list 1 2 3 4 5 6 7 8 9 10 0))
(memory-set! 0 0)                       ; initialize
(hrm-execute! program-add-converted)

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
