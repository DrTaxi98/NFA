;;;;    nfa.lisp: regular languages compiler and NFA tester
;;;;    Copyright (C) 2020 Andrea Tassi
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program. If not, see <https://www.gnu.org/licenses/>.
;;;;
;;;;    Author's email address: andreatassi98@gmail.com

;; T if functor is reserved, nil otherwise.
(defun is-reserved-functor (functor)
  (or (eql functor 'seq)
      (eql functor 'or)
      (eql functor 'star)
      (eql functor 'plus)))

;; Returns a function that controls if the arguments
;; of functor respect its arity.
(defun functor-arity (functor)
  (cond ((or (eql functor 'seq) (eql functor 'or))
	 (lambda (arity) (>= arity 2)))
	((or (eql functor 'star) (eql functor 'plus))
	 (lambda (arity) (= arity 1)))))

;; T if symbol is a symbol of the alphabet, nil otherwise.
(defun is-alphabetical-symbol (symbol)
  (cond ((atom symbol) T)
	(T (not (is-reserved-functor (first symbol))))))

;; T if RE is a regular expression, nil otherwise.
(defun is-regexp (RE)
  (if (is-alphabetical-symbol RE)
      T
    (and (funcall (functor-arity (first RE)) (length (rest RE)))
	 (car (mapcar 'is-regexp (rest RE))))))

;; Returns the list of the deltas of the automata FA.
(defun deltas-list (FA)
  (rest (rest FA)))

;; Returns the automata obtained by the RE compilation,
;; if it is a regexp, nil otherwise.
;; If it can't compile RE, it returns nil.
(defun nfa-regexp-comp (RE)
  (if (is-regexp RE)
      (thompsons-construction RE)
    nil))

;; Returns the automata built with the
;; Thompson's Construction for the regexp RE.
(defun thompsons-construction (RE)
  (cond ((atom RE)
	 (thompsons-construction-symbol RE))
	((eql (first RE) 'seq)
         (thompsons-construction-seq (rest RE)))
	((eql (first RE) 'or)
	 (thompsons-construction-or (rest RE)))
	((eql (first RE) 'star)
	 (thompsons-construction-star (rest RE)))
	((eql (first RE) 'plus)
	 (thompsons-construction-plus (rest RE)))
	(T (thompsons-construction-symbol RE))))

;; Returns the automata built with the
;; Thompson's Construction to recognise symbol.
(defun thompsons-construction-symbol (symbol)
  (let ((initial (gensym "q"))
	(final (gensym "q")))
    (append (list initial)
	    (list final)
	    (list (list initial symbol final)))))

;; Returns the automata built with the
;; Thompson's Construction for the regexp
;; with functor 'seq' and arguments args.
(defun thompsons-construction-seq (args)
  (if (null (rest args))
      (nfa-regexp-comp (first args))
    (let ((first-nfa (nfa-regexp-comp (first args)))
          (second-nfa (thompsons-construction-seq (rest args))))
      (append (list (first first-nfa))
	      (list (second second-nfa))
	      (deltas-list first-nfa)
	      (list (list (second first-nfa) 'epsilon (first second-nfa)))
	      (deltas-list second-nfa)))))

;; Returns the automata built with the
;; Thompson's Construction for the regexp
;; with functor 'or' and arguments args.
(defun thompsons-construction-or (args)
  (if (null (rest args))
      (nfa-regexp-comp (first args))
    (let ((initial (gensym "q"))
	  (first-nfa (nfa-regexp-comp (first args)))
	  (second-nfa (thompsons-construction-or (rest args)))
	  (final (gensym "q")))
      (append (list initial)
	      (list final)
	      (list (list initial 'epsilon (first first-nfa)))
	      (list (list initial 'epsilon (first second-nfa)))
	      (deltas-list first-nfa)
	      (deltas-list second-nfa)
	      (list (list (second first-nfa) 'epsilon final))
	      (list (list (second second-nfa) 'epsilon final))))))

;; Returns the automata built with the
;; Thompson's Construction for the regexp
;; with functor 'star' and argument arg.
(defun thompsons-construction-star (arg)
  (let ((nfa (thompsons-construction-plus arg)))
    (append nfa
	    (list (list (first nfa) 'epsilon (second nfa))))))

;; Returns the automata built with the
;; Thompson's Construction for the regexp
;; with functor 'plus' and arguments arg.
(defun thompsons-construction-plus (arg)
  (let ((initial (gensym "q"))
	(nfa (nfa-regexp-comp (first arg)))
	(final (gensym "q")))
    (append (list initial)
	    (list final)
	    (list (list initial 'epsilon (first nfa)))
	    (deltas-list nfa)
	    (list (list (second nfa) 'epsilon final))
	    (list (list (second nfa) 'epsilon (first nfa))))))

;; T if Input is a list of symbols of the alphabet,
;; nil otherwise.
(defun is-alphabetical-list (Input)
  (if (null Input) T
    (and (is-alphabetical-symbol (first Input))
       (is-alphabetical-list (rest Input)))))

;; T if FA has the structure of an automata,
;; nil otherwise.
(defun is-nfa (FA)
  (if (atom FA) nil
    (and (>= (length FA) 3)
	 (atom (first FA))
	 (atom (second FA))
	 (is-deltas-list (deltas-list FA)))))

;; T if deltas is a list of delta-transitions,
;; nil otherwise.
(defun is-deltas-list (deltas)
  (if (= (length deltas) 1)
      (is-delta (first deltas))
    (and (is-delta (first deltas))
	 (is-deltas-list (rest deltas)))))

;; T if delta is a delta-transition,
;; nil otherwise.
(defun is-delta (delta)
  (if (atom delta) nil
    (and (= (length delta) 3)
	 (atom (first delta))
	 (is-alphabetical-symbol (second delta))
	 (atom (third delta)))))

;; T if Input for the automata FA is completely consumed
;; and the automata is in a final state.
;; Input is a Lisp list of symbols of the alphabet.
;; If FA has not the correct structure of an automata,
;; it reports an error.
;; Otherwise, it returns T if FA recognises Input,
;; nil if it doesn't.
(defun nfa-test (FA Input)
  (cond ((and (atom Input) (not (null Input)))
	 nil)
	((not (is-alphabetical-list Input))
	 nil)
	((not (is-nfa FA))
	 (format *standard-output*
		 "Error: ~S is not a Finite State Automata.~%"
		 FA))
	(T (nfa-accept FA Input (first FA)))))

;; T if Input for the automata FA in the current-state
;; is completely consumed and the automata
;; is in a final state.
(defun nfa-accept (FA Input current-state)
  (let ((next-state (first (apply-delta (deltas-list FA)
					(first Input)
					current-state)))
	(epsilon-transitions (apply-delta (deltas-list FA)
					  'epsilon
					  current-state)))
    (cond ((not (null epsilon-transitions))
	   (let ((accept-transition
		  (nfa-accept FA Input (first epsilon-transitions))))
	     (if (not (null accept-transition))
		 accept-transition
	       (nfa-accept FA Input (second epsilon-transitions)))))
	  ((null Input)
	   (eql current-state (second FA)))
	  ((eql (first Input) 'epsilon)
	   (nfa-accept FA (rest Input) current-state))
	  ((null next-state)
	   nil)
	  (T (nfa-accept FA (rest Input) next-state)))))

;; Returns a list of all the possible states
;; that are reachable through all the possible
;; delta-transitions from the list deltas
;; that accept symbol starting from state.
(defun apply-delta (deltas symbol state)
  (let ((first-delta (first deltas)))
    (cond ((null deltas)
	   nil)
	  ((and (eql state (first first-delta))
		(equal symbol (second first-delta)))
	   (append (list (third first-delta))
		   (apply-delta (rest deltas) symbol state)))
	  (T (apply-delta (rest deltas) symbol state)))))

;;;; eof nfa.lisp
