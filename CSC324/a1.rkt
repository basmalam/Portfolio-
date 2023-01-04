#lang racket #| * CSC324H5 Fall 2021: Assignment 1 * |#
#|
Module:        a1
Description:   Assignment 1: Checking for Tail Recursion
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2021
|#

; This specifies which functions this module exports. Don't change this!
(provide is-tail-recursive)

; Import the testing library
(module+ test
  (require rackunit))


#|
(is-tail-recursive def) -> boolean? 
  def: (and/or symbol? number?) 
    A function definition that follows the grammar for <def> in the handout.
    You may assume that the function defined in def is recursive.

  Returns whether the definition is tail recursive
|#
(define (is-tail-recursive def)
  (let* ([fname (first (second def))]
         [body  (third def)])
     (in-tail-position body fname))) ; TODO: replace (void) with your code

(define/match (has-call expr func)
  [ ('() func) #f]
  [((cons x xs) func) (if (list? x) (or (has-call x func)(has-call xs func)) (if (equal? x func) #t (has-call xs func)))]
  [(expr func) (if (equal? expr func) #t #f)]
  )

 (define/match (in-tail-position expr func)
  [((cons 'if xs) func)  (if (has-call (first xs) func) #f (in-tail-position (last xs) func))]
  [((cons 'let* xs) func) (if (has-call (first xs) func) #f (in-tail-position (last xs) func))]
  [((cons x xs) func) (if (and (equal? x func) (equal? (rest xs) '()) (not(has-call xs func))) #t
                          (if (and (has-call x func) (not(equal? xs '()))) #f
                              (if (and (has-call x func) (equal? xs '())) #t (in-tail-position xs func))))])


(module+ test
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "Simple test for has-call"        ; Test label
               (has-call '(f x) 'f)  ; Actual value
               #t); Expected value
                                            ; Expected value
  (test-equal? "Simple test for has-call w/ if"        ; Test label
               (has-call '(if (() g h (g (f x)) x x)) 'f)  ; Actual value
               #t); Expected value

    (test-equal? "in-tail-pos"        ; Test label
               (in-tail-position '(if (() g h (g (f x)) x x)) 'f)  ; Actual value
               #f); Expected value
                                    


  ; TODO: Write more tests. Testing is an important part of programming,
  ; so you and your partner must write your own tests. Do not share your
  ; tests with anyone else.
)

; You can write helper functions freely
(module+ test
  ; We use rackunit's test-equal? to define some simple tests.
  (test-equal? "Simple test for tail recursion"        ; Test label
               (is-tail-recursive '(def (f x) (f x)))  ; Actual value
               #t)                                     ; Expected value
  (test-equal? "Simple test for tail recursion"        ; Test label
               (is-tail-recursive '(def (f x) ((g x)(f x)(g x))))  ; Actual value
               #f)  
  (test-equal? "Recursive call in if expression conditional"
               (is-tail-recursive '(def (f x) (if (f x) x x)))
               #f)
  (test-equal? "Recursive call in if expression conditional"
               (is-tail-recursive '(def (f x) (if (f x) (f x) (f x))))
               #f)
  (test-equal? "Recursive call in if expression conditional"
               (is-tail-recursive '(def (f x) (if (f x) (f x) (f x))))
               #f)
  (test-equal? "Recursive call in let* definition"
               (is-tail-recursive '(def (f x) (let* ([a (f x)]) (g a))))
               #f)
  (test-equal? "Recursive call in let* body"
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) ((f a) (g a)))))
               #f)
  (test-equal? "Recursive call in let* body"
               (is-tail-recursive '(def (f x) (let* ([a (f x)]) (g a))))
               #f)
  (test-equal? "Recursive call multiple recursive calls"
               (is-tail-recursive '(def (f x) ((f a) (g x) (d a))))
               #f)
    (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (if x (f b)))))
               #t)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (if (f x) (f b)))))
               #f)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (let* ([a (g x)]) (f x)))))
               #t)
   (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (let* ([a (g x)]) (g (d (f a)) x)))))
               #f)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (if (f a) x))))
               #f)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (if (f a) (f x)))))
               #f)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (if (if (g a) a) (f x)))))
               #t)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g(f x))]) (if (if (g a) a) (f x)))))
               #f)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g(g x))]) (if (if (g a) a) (f x)))))
               #t)
   (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g(g x))]) (if (if (g a) a) (f x)))))
               #t)

  (test-equal? "Recursive call embeded function "
               (is-tail-recursive '(def (f x) (a (f x) b (f x))))
               #f)
   (test-equal? "Recursive call embeded function "
               (is-tail-recursive '(def (f x) (a (f(f x)) b (f x))))
               #f)
  (test-equal? "Recursive call embeded function "
               (is-tail-recursive '(def (f x y) (f (d (f a b)) c)))
               #f)
  (test-equal? "Recursive call embeded function "
               (is-tail-recursive '(def (f x) (a (g x) b (f x))))
               #t)
   (test-equal? "end "
               (is-tail-recursive '(def (f x) (g (d (h b)) (f x))))
               #t)
  (test-equal? "Recursive call embeded function "
               (is-tail-recursive '(def (f x) (f(f x))))
               #f)
   (test-equal? "Recursive call embeded function "
               (is-tail-recursive '(def (f x) (g (f x))))
               #t)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (let* ([a (g x)]) (if (if (g a) (f a)) (f a)))))
               #f)
  (test-equal? "Recursive call embeded if "
               (is-tail-recursive '(def (f x) (if (let* ([a (g(g x))]) (if x (g x))) a (f x))))
               #t)
  

  ; TODO: Write more tests. Testing is an important part of programming,
  ; so you and your partner must write your own tests. Do not share your
  ; tests with anyone else.
)

