#lang plai-typed
;EXERCICE 1
;1.1
(define (somme [L : (listof number)]) : number
  (foldr + 0 L))

;TESTS
(test (somme (list 0 1 2 3 4)) 10)
(test (somme (list 4 7 8)) 19)

;1.2
(define (Append [LG : (listof 'a)]
                [LD : (listof 'a)]) : (listof 'a)
  (foldr cons LD LG))

;TESTS
(define list1 (list 0 1 2))
(define list2 (list 3 4 5))
(test (Append list1 list2) (append list1 list2))

;1.3
(define (Map proc [L : (listof 'a)]) : (listof 'a)
  (local [(define (iter [L : (listof 'a)] [acc : (listof 'a)]) : (listof 'a)
            (if (empty? L)
                acc
                (iter (rest L) (cons (proc (first L)) acc))))]
    (reverse (iter L empty))))

(define (Foldl proc init [L : (listof 'a)]) : 'a
  (local [(define (iter [L : (listof 'a)] [acc : 'a]) : 'a
            (if (empty? L)
                acc
                (iter (rest L) (proc (first L) acc))))]
    (iter L init)))

(define (Foldr proc init [L : (listof 'a)])
  (reverse (Foldl proc init L)))

;TESTS
(test (Map (lambda (x) (+ 1 x)) list1) (map (lambda (x) (+ 1 x)) list1))
(test (Foldl + 0 list2) (foldl + 0 list2))
(test (Foldr cons empty list1) (foldr cons empty list1))

;EXERCICE 2
(define-type Couple
  [couple (first : 'a) (second : 'a)])

(define-type Optionof
  [vide]
  [element (item : 'a)])

(define (find [L : (listof Couple)] [key : 'a]) : Optionof
  (if (empty? L)
      (vide)
      (if (equal? key (couple-first (first L)))
          (element (couple-second (first L)))
          (find (rest L) key))))

;TESTS
(define couples (list (couple 'x 3) (couple 'y 7)))
(test (find couples 'y) (element 7))
(test (find couples 'z) (vide))

;EXERCICE 3
(define-type ArbreBinaire
  [feuille (value : number)]
  [noeud (evaluate : (number number -> number)) (fg : ArbreBinaire) (fd : ArbreBinaire)])

(define (eval [T : ArbreBinaire]) : number
  (cond [(feuille? T) (feuille-value T)]
        [(noeud? T) ((noeud-evaluate T) (eval (noeud-fg T)) (eval (noeud-fd T)))]))

;TESTS
;(* 3 (+ 1 2))
(define arbre1 (noeud * (feuille 3) (noeud + (feuille 1) (feuille 2))))
(test (eval arbre1) (* 3 (+ 1 2)))

;(* (* 3 2) (+ 1 1))
(define arbre2 (noeud * (noeud * (feuille 2) (feuille 3)) (noeud + (feuille 1) (feuille 1))))
(test (eval arbre2) (* (* 3 2) (+ 1 1)))
