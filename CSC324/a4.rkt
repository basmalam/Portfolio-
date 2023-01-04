#lang racket #| * CSC324H5 Fall 2021: Assignment 4 * |#
#|
Module:        a4
Description:   Assignment 4: Theorem Proving in miniKanren
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021
|#

; Do not add additional imports
(require "mk.rkt")
(require racket/control)

; This specifies which functions this module exports. Don't change this!
(provide proof?
         my-proof
         proofo
         prove)

; Import the testing library
(module+ test
  (require rackunit))

;-------------------------------------------------------------------------------
; Task 1: Using a proof checker
;-------------------------------------------------------------------------------

#|
(proof? prop proof) -> bool?
  prop: (and/or list? symbol?)
    A proposition that we wish to prove
  proof: list?
    A proof for that proposition

  Returns a boolean describing whether `proof` is a valid proof of `prop`
|#
(define (proof? prop proof)
  (equal? (proof/asmpt proof '()) ; check what proposition 'proof' proves
          prop))                  ; and check if it is equal to 'prop'

#|
(proof/asmpt proof asmpts) -> (and/or bool? list?)
  proof: list?
    A proof for that proposition
  asmpt: list?
    A list of currently valid assumptions

  Return either the proposition that a proof is proving given the assumption,
  or #f is the proof is invalid.
|#
(define/match (proof/asmpt proof asmpt)
  [((list 'use p) asmpt)
   (if (member p asmpt) p #f)]
  [((list 'assume hypo subproof) asmpt)
   (list hypo '-> (proof/asmpt subproof (cons hypo asmpt)))]
  [((list 'modus-ponens subpf1 subpf2) asmpt)
   (let* ([prop2 (proof/asmpt subpf2 asmpt)])
     (match prop2            ; "match" is like "case ... of" in Haskell
       [(list hypo '-> conc) ; and does pattern matching on "prop2"
        (if (equal? (proof/asmpt subpf1 asmpt) hypo)
            conc
            #f)]
       [prop2 #f]))])


#|
my-theorem: A theorem that you will prove.
|#
(define my-theorem '((A -> (B -> C)) -> (B -> (A -> C))))

#|
my-proof: A proof for my-theorem: (proof? my-proof my-theorem)
should return true.
|#
(define my-proof '(assume (A -> (B -> C)) (assume B (assume A (modus-ponens (use B) (modus-ponens (use A) (use (A -> (B -> C)))))))))

(module+ test
  (test-equal? "my-proof proves my-theorem"
               (proof? my-theorem my-proof)
               #t)
  ; Some additional tests for the proof checker that are provided to you
  ; These are included to illustrate how to write proofs.

  #;(test-equal? "Example 1 from the handout"
               (proof? '(A -> A)
                       '(assume A (use A)))
               #t)
  #;(test-equal? "Example 2 from the handout"
               (proof? '(A -> ((A -> B) -> B))
                       '(assume A (assume (A -> B) (modus-ponens (use A) (use (A -> B))))))
               #t)
  #;(test-equal? "Example 3 from the handout"
               (proof? '((C -> D) -> (C -> D))
                       '(assume (C -> D) (assume C (modus-ponens (use C) (use (C -> D))))))
               #t)
  #;(test-equal? "Example 4 from the handout"
               (proof? '((C -> D) -> (C -> D))
                       '(assume (C -> D) (use (C -> D))))
               #t)
)

;-------------------------------------------------------------------------------
; Task 2: A Proof Checking Relation
;-------------------------------------------------------------------------------

#|
(proofo prop proof)
  prop:  A proposition following the grammar specified in the handout
  proof: A proof following the grammar specified in the handout

  The relational form of the `proof?` function. Succeeds when "proof"
  is a correct proof of the proposition "prop".
|#
(define (proofo prop proof)
  (proof-helpero prop proof '()))

#|
(proof-helpero prop proof assmpts)
  prop:    A proposition following the grammar specified in the handout
  proof:   A proof following the grammar specified in the handout
  assmpts: A list of assumptions propositions

  The relational form of the `proof/asmpt` function. Succeeds when "proof"
  is a correct proof of the proposition "prop" given the list of
  assumptions "assmpts".
|#

(define (proof-helpero prop proof assmpts)
  (conde
   ((fresh (hypo subproof)
           (== proof (list 'use hypo))
           (membero hypo assmpts)
           ))
   ((fresh (hypo subproof subprop)
           (== proof (list 'assume hypo subproof))
           (== prop (list hypo '-> subprop))
           (proof-helpero subprop subproof (append assmpts (list hypo)))
          ))
   ((fresh (prof1 prof2 x y)
           (== proof (list 'modus-ponens prof1 prof2))
           (== prof1 (list 'use x))
           (== prof2 (list 'use (list x '-> y)))
           (== prop y)
   ))
     ((fresh (prof1 prof2 prof11 prof22 x y)
           (== proof (list 'modus-ponens prof1 prof2))
           (== prof1 (list 'use x))
           (proof-helpero prop prof2 assmpts)
   ))

   ))
 

            ; TODO REPLACE THIS!

#|
(membero x lst)
  x:   A value
  lst: A list

  The relational form of the `member` function. Succeeds when "x"
  is an element of the list "lst". You may use this relation as a helper
  in your implementation of "proof-helpero".
|#
(define (membero x lst)
  (fresh (first rest)
    (== lst (cons first rest))
    (conde ((== first x))
           ((=/= first x)
            (membero x rest)))))


; Uncomment these to test your proofo relation
(module+ test
  (test-equal? "Example 1 from the handout"
               (run 1 (q) (proofo '(A -> A) '(assume A (use A))))
               '(_.0))
  (test-equal? "Example 2 from the handout"
               (run 1 (q) (proofo '(A -> ((A -> B) -> B))
                                  '(assume A (assume (A -> B) (modus-ponens (use A) (use (A -> B)))))))
               '(_.0))

  (test-equal? "Example of an incorrect proof"
               (run 1 (q) (proofo '(A -> ((A -> B) -> B))
                                  '(assume (A -> B) (assume A (modus-ponens (use A) (use (A -> B)))))))
               '())
  (test-equal? "our case"
               (run 1 (q) (proofo '((A -> (B -> C)) -> (B -> (A -> C)))
                                  '(assume (A -> (B -> C)) (assume B (assume A (modus-ponens (use A) (modus-ponens (use B) (use (A -> (B -> C))))))))))
               '(_.0))
  (test-equal? "our case"
               (run 1 (q) (proofo '((C -> D) -> (C -> D))
                                  '(assume (C -> D) (assume C (modus-ponens (use C) (use (C -> D)))))))
               '(_.0))

   
  ; Write more tests here
)

;-------------------------------------------------------------------------------
; Task 3: A Theorem Prover
;-------------------------------------------------------------------------------

#|
(prove prop)
  prop:  A proposition following the grammar specified in the handout

  Returns a proof of the proposition, if a proof exists. Otherwise,
  this function returns `#f` or may fail to terminate.
|#
(define (prove prop)
  (phelper prop '() (phelper1 prop)))


(define/match (phelper1 prop)
  [((list hypo '-> subproof)) (if (and (and (list? hypo) (list? subproof)) (and (list? (first hypo)) (list? (first subproof)))
                                       (equal? (last hypo) (last subproof))) (list (first hypo) '-> (first subproof)) #f)])
; prevent duplicates from statements 
(define/match (phelper prop statments prop2)
  [((list hypo '-> subproof) s prop2) (let* ([result (phelper subproof (append s (list hypo)) prop2)])
                                  (cond
                                    [(equal? result #f) #f]
                                    [else (list 'assume hypo result )]))]
  [(x s prop2) (cond
           [(list? prop2) (phelper prop2 '() (phelper1 prop2))]
           [(check_s x s) (append '(use) (list x))]
           [else (let* ([mtch (getmatch x s)])(cond
                                                [(equal? mtch #f) #f]
                                                         [else 
                                                          (let* ([lst (append (remove* '(->) (remove x (reverse (flatten mtch)))) (list x))])
                                                        (cond
                                                          [(and (equal? (rest lst) (list x)) (member (first lst) s))  (list 'modus-ponens (list 'use (first lst)) (list 'use mtch))]
                                                          [else (make-modus x lst s mtch )]))]))])])
  
(define/match (make-modus prop lst s fst)
  [(prop (list x prop) s fst) (if (check_s x s) (list (list 'modus-ponens (list 'use x) (list 'use fst))) (append (append (list 'modus-ponens) (let* ([mtch (getmatch x s)])
                                  (cond
                                    [(equal? mtch #f) #f]
                                    [else
                                     (let* ([lst (append (remove* '(->) (remove x (reverse (flatten mtch)))) (list x))])
                                       (make-modus x lst s mtch))]))) (list (list 'use fst))))]
  
  [(prop (cons x xs) s fst) (if (check_s x s) (append (list 'modus-ponens (list 'use x)) (make-modus prop xs (remove x s) fst))
                                (let* ([mtch (getmatch x s)])
                                  (cond
                                    [(equal? mtch #f) #f]
                                    [else
                                     (let* ([lst (append (remove* '(->) (remove x (reverse (flatten mtch)))) (list x))])
                                       (make-modus x lst s mtch))])))])
                                     
                           

(define/match (getmatch prop lst)
  [(prop '()) #f]
  [(prop (cons x xs)) (if (equal? (last (flatten x)) prop) x (getmatch prop xs))])

   
(define (reverse lst)
  (cond [(equal? lst '())  ; base case condition todo
         '()] ; base case body todo
        [else
         (let* ([lst-first (first lst)]
                [lst-rest  (rest lst)])
           (append (reverse lst-rest)      ; recursive case todo
                   (list lst-first)))]))
                                                                                                                                                                               

(define/match (last lst)
  [((cons x xs)) (if (equal? xs '()) x (last xs))]
  [(x) x]
  )

(define (flatten lst)
  (cond
  [(empty? lst) '()]
  [(list? lst) (append (flatten (first lst)) (flatten (rest lst)))]
  [else (list lst)]))
  

(define/match (check_s p statements)
  [(p '()) #f]
  [(p (cons x xs)) (if (equal? x p) #t (check_s p xs))])
  
; pair finding function 


; Uncomment these to test your proofo relation
 (module+ test
  (test-equal? "Example 1 from the handout"
               (prove '(A -> A))
               '(assume A (use A)))
   (test-equal? "Example 1 from the handout"
               (prove '((C -> D) -> (C -> D)))
                '(assume (C -> D) (assume C (modus-ponens (use C) (use (C -> D))))))
     (test-equal? "Example 2 from the handout"
               (prove  '(A -> ((A -> B) -> B)))
               '(assume A (assume (A -> B) (modus-ponens (use A) (use (A -> B)))))
               )
   (test-equal? "a1t"
               (prove  '(A -> B))
               #f
               )
    (test-equal? "Example 2 from the handout"
               (prove  '(A -> (B -> A)))
               '(assume A (assume B (use A)))
               )
   (test-equal? "Example 2 from the handout"
               (prove  '(A -> (B -> B)))
               '(assume A (assume B (use B)))
               )
    (test-equal? "23"
               (prove '(((A -> (B -> C)) -> C) -> ((B -> (A -> C)) -> C)))
               '(assume
    ((A -> (B -> C)) -> C)
    (assume
     (B -> (A -> C))
     (assume
      (A -> (B -> C))
      (assume
       B
       (assume
        A
        (modus-ponens (use B) (modus-ponens (use A) (use (A -> (B -> C))))))))))
               )
    (test-equal? "at"
               (prove '((A -> B) -> ((B -> C) -> (A -> C))))
              '(assume
    (A -> B)
    (assume
     (B -> C)
     (assume
      A
      (modus-ponens (modus-ponens (use A) (use (A -> B))) (use (B -> C))))))
               )
        (test-equal? "our example"
               (prove '((A -> (B -> C)) -> (B -> (A -> C))))
               '(assume (A -> (B -> C)) (assume B (assume A (modus-ponens (use B) (modus-ponens (use A) (use (A -> (B -> C)))))))))


  ; Be careful when you write new tests, since they may be slow
)
