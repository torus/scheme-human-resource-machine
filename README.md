# Human Resource Machine Simulator

Simulator for [Human Resource Machine](https://tomorrowcorporation.com/humanresourcemachine).

### System Requirements

- Gauche Scheme interpretor 0.9.5 or above

### Example

#### Just add 2 numbers
```scheme
(define program-add
  `((,hrm-inbox!)
    (,hrm-copyto! 0)
    (,hrm-inbox!)
    (,hrm-add! 0)
    (,hrm-outbox!)))
```

#### Program with jump

Since this simulator doesn't have the program counter, jump destination is specified by the list to execute. To make a loop, I used `set-cdr!` in the program. (So, it's literary looping!)

```scheme
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
```
