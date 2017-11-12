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
