;;; Copyright 1995 Tracy Gardner & University of Bath
;;; Copyright 2010 Henry G. Weller
;;;-----------------------------------------------------------------------------
;;  This file is part of
;;; ---                         EuLisp System 'Youtoo'
;;;-----------------------------------------------------------------------------
;;
;;  Youtoo is free software: you can redistribute it and/or modify it under the
;;  terms of the GNU General Public License version 2 as published by the Free
;;  Software Foundation.
;;
;;  Youtoo is distributed in the hope that it will be useful, but WITHOUT ANY
;;  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;  details.
;;
;;  You should have received a copy of the GNU General Public License along with
;;  this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;-----------------------------------------------------------------------------
;;; Title: More condition element stuff.
;;;  Library: ops5
;;;  Authors: Tracy Gardner
;;;  Maintainer: Henry G. Weller
;;;  Description:
;;    OPS5 for EuLisp System 'youtoo'
;;;-----------------------------------------------------------------------------

(defmodule cond-el-2
  (syntax (syntax-0
           macros-tag)
   import (level-0
           basic
           cond-el-gf
           cond-el-1
           tests
           prod-gf
           wm-gf
           ops5-def
           ops-out)
   export (make-pos-join-ce
           make-pos-njoin-ce
           make-neg-join-ce
           make-neg-njoin-ce
           ce-c-tests
           ce-v-tests
           ce-j-tests
           set-ce-c-tests
           set-ce-v-tests
           set-ce-j-tests
           <pos-join-ce>
           <neg-join-ce>
           <pos-njoin-ce>
           <neg-njoin-ce>
           <condition-element>
           ce-num-matched
           set-ce-num-matched
           ce-matches
           set-ce-matches
           ce-prods
           set-ce-prods
           var-bindings
           join-list
           test-succeeds
           ce-id))

(print "### cond-el-2" nl)

;;;-----------------------------------------------------------------------------
;;; Condition Element Manager Stuff
;;;-----------------------------------------------------------------------------
(defmethod print-ces ((ce-man <ce-manager>))
  (sformat ops-out  "~a Condition Elements: ~%" (size (cond-els ce-man)))
  (letfuns ((loop (ces)
                 (cond
                   ((null? ces))
                   (t
                    (sformat ops-out "Class: ~a ~a~%" (class-of (caar ces))
                             (size (cadar ces)))
                    (do
                     (lambda (x) (print x nl))
                     (cadar ces))
                    (loop (cdr ces))))))
          (loop (cond-els ce-man))))

;;;-----------------------------------------------------------------------------
;;; returns either the modified ce manager or the pre-existing
;;; identical ce.
;;;-----------------------------------------------------------------------------
(defmethod insert-new-ce ((ce-manager <ce-manager>)
                          (new-ce <condition-element>) prod)
  ;;(print "insert-new-ce" nl)
  (let* ((ces (member-alist (ce-class-name new-ce) (cond-els ce-manager)))
         (res (exists (if ces (cadr ces) ()) new-ce))
         (ce  (if res res (add-new-ce ce-manager new-ce))))
    (set-ce-prods ce (cons prod (ce-prods ce)))
    ce))

(defun add-new-ce (ce-manager new-ce)
  ;;(sformat ops-out "Adding new ce~%")
  ;;(sformat ops-out "Existing ces: ~a~%" (cond-els ce-manager))
  (let* ((old-ces (cond-els ce-manager))
         (class (ce-class-name new-ce))
         (new-class (null? (member-alist class old-ces))))
    (if new-class
        (set-cond-els ce-manager
                      (cons (list class (list new-ce))
                            (cond-els ce-manager)))
      (set-cond-els ce-manager
                    (map (lambda (x)
                           (if (binary= (car x) class)
                               (cons (car x)
                                     (list (cons new-ce
                                                 (cadr x))))
                             x))
                         (cond-els ce-manager)))))
  (set-ce-rating new-ce (calc-rating new-ce))
  (set-ce-id new-ce (next-id ce-manager))
  (set-next-id ce-manager (+ (next-id ce-manager) 1))
  new-ce)

(defgeneric calc-rating ((ce <condition-element>))
  method: (((ce <pos-join-ce>))
           (+ (size (ce-c-tests ce)) (size (ce-j-tests ce))))
  method: (((ce <pos-njoin-ce>))
           (size (ce-c-tests ce)))
  method: (((ce <neg-join-ce>))
           (+ (size (ce-c-tests ce)) (size (ce-j-tests ce))))
  method: (((ce <neg-njoin-ce>))
           (size (ce-c-tests ce))))

(defun exists (cond-els ce)
  ;;(print "exists" nl)
  (letfuns ((loop (rest)
                 (cond
                   ((null? rest) ())
                   (t
                    (let ((curr (car rest)))
                      (if (and (eql (class-of ce) (class-of curr))
                               (binary= (ce-c-tests ce) (ce-c-tests curr))
                               (binary= (ce-v-tests ce) (ce-v-tests curr))
                               (if (or (eql (class-of ce) <pos-join-ce>)
                                       (eql (class-of ce) <neg-join-ce>))
                                   (binary= (ce-j-tests ce) (ce-j-tests curr))
                                 t))
                          curr
                        (loop (cdr rest))))))))
          (loop cond-els)))

;;;-----------------------------------------------------------------------------
;;; Match Process Stuff
;
; match-insert
;   Checks to see if a newly inserted WM-Element matches this
;   condition element. If so then information on WM-elements that
;   match the condition element is updated and the productions that
;   use this condition element are informed of the change.
;;;-----------------------------------------------------------------------------
(defmethod match-insert ((ce-manager <ce-manager>) wme cr-manager)
  ;;(format "match-insert: ~a" (wme-attrib-vals wme))
  (let* ((class (wme-class-name wme))
         (ces (cadr (member-alist class (cond-els ce-manager)))))
    ;;(format "Ces to inform: ~a~%" ces)
    (do
     (lambda (x)
       ;;(print x nl)
       (match-insert-ce wme x cr-manager))
     ces)))

(defmethod match-insert-ce (wme (ce <pos-join-ce>) cr-manager)
  ;;(print "match-insert-ce" nl)
  (when (match-insert-join wme ce)
        (inform-create-prod-insts (wme-timestamp wme) ce
                                  (join-list ce (wme-timestamp wme))
                                  cr-manager)))

(defmethod match-insert-ce (wme (ce <neg-join-ce>) cr-manager)
  (when (match-insert-join wme ce)
        (inform-remove-prod-insts (wme-timestamp wme) ce cr-manager)))

(defmethod match-insert-ce (wme (ce <pos-njoin-ce>) cr-manager)
  (when (match-insert-njoin wme ce)
        (inform-create-prod-insts (wme-timestamp wme) ce () cr-manager)))

(defmethod match-insert-ce (wme (ce <neg-njoin-ce>) cr-manager)
  (when (match-insert-njoin wme ce)
        (inform-remove-prod-insts (wme-timestamp wme) ce cr-manager)))

(defun match-insert-njoin (wme ce)
  ;;(print "match-insert-njoin" nl)
  (let ((ret (const-match wme ce)))
    (when ret         ; wme passes constant tests
          (let ((v-bindings (compute-var-bindings-njoin wme ce)))
            (set-ce-num-matched ce (+ (ce-num-matched ce) 1))
            (set-ce-matches ce (cons (cons (wme-timestamp wme)
                                           (list v-bindings ()))
                                     (ce-matches ce)))))
    ret))


(defun match-insert-join (wme ce)
  ;;(format "match-insert-join")
  ;;(print ce nl)
  ;;(print wme nl)
  (let ((ret (const-match wme ce)))
    (when ret           ; wme passes constant tests
          (let ((v-bindings (compute-var-bindings-join wme ce))
                (jv-tests (compute-jv-tests wme ce)))
            (set-ce-num-matched ce (+ (ce-num-matched ce) 1))
            (set-ce-matches ce (cons (cons (wme-timestamp wme)
                                           (list v-bindings jv-tests))
                                     (ce-matches ce)))))
    ;;(print ret nl)
    ret))

(defun compute-jv-tests (wme ce)
  ;;(print "compute-jv-tests" nl)
  ;;(print (ce-j-tests ce) nl)
  (accumulate
   (lambda (a test)
     (let* ((val (member-alist (test-attrib test) (wme-attrib-vals wme)))
            (a-val (if (null? val) 'NIL (cdr val))))
       ;;(print (test-attrib test) nl)
       ;;(print (wme-attrib-vals wme) nl)
       ;;(print a-val nl)
       (cons (cons a-val (list (list (test-pred test) (test-var test)))) a)))
   ()
   (ce-j-tests ce)))

(defun compute-var-bindings-njoin (wme ce)
  ;;(print "compute-var-bindings-njoin" nl)
  (accumulate
   (lambda (a test)
     (if (eql (test-pred test) '=)
         (cons
          (cons (test-value test)
                (let ((val (member-alist (test-attrib test)
                                         (wme-attrib-vals wme))))
                  (if (null? val) 'NIL (cdr val))))
          a)
       a))
   ()
   (ce-v-tests ce)))

(defun compute-var-bindings-join (wme ce)
  ;;(print "compute-var-bindings-join" nl)
  (let ((bindings (accumulate
                   (lambda (a test)
                     (let* ((val (member-alist (test-attrib test)
                                               (wme-attrib-vals wme)))
                            (a-val (if (null? val) 'NIL (cdr val))))
                       (if (binary= (test-pred test) '=)
                           (cons (cons (test-value test) a-val) a)
                         a)))
                   ()
                   (append (ce-v-tests ce) (ce-j-tests ce)))))
    ;;(format "So far: ~a~%" bindings)
    (accumulate
     (lambda (a test)
       (let* ((join-var (test-value test))
              (attrib (test-attrib test))
              (val (member-alist attrib (wme-attrib-vals wme)))
              (jv-value (if (null? val) 'NIL (cdr val))))

         (if (null? (member-alist join-var (ce-jv-vals ce)))
             (set-ce-jv-vals
              ce
              (cons (cons join-var
                          (list (cons jv-value (wme-timestamp wme))))
                    (ce-jv-vals ce)))
           (set-ce-jv-vals
            ce
            (map
             (lambda (x)
               (if (eql (car x) join-var)
                   (cons (car x)
                         (cons
                          (cons jv-value
                                (wme-timestamp wme))
                          (cdr x)))
                 x))
             (ce-jv-vals ce))))
         (cons
          (cons join-var jv-value) a))
       a)
     bindings
     (ce-j-tests ce))))

(defun const-match (wme ce)
  ;;(print "const-match" nl)
  (let ((const-tests (ce-c-tests ce)))
    (letfuns ((test (tests)
                   (cond
                     ((null? tests) t)
                     (t
                      (let ((passes (passes-test wme (car tests))))
                        ;;(when (null? passes)
                        ;;(format "Failed test: ~a~a~%"
                        ;;       wme (car tests)))
                        (if passes (test (cdr tests)) ()))))))
            (test const-tests))))

;; passes-test
;; Checks whether working memory element, wme, passes the constant test, test.
;;
(defun passes-test (wme test)
  ;;(format "passes-test: ~a~%" test)
  (let* ((attrib (test-attrib test))
         (val (member-alist attrib (wme-attrib-vals wme)))
         (wme-val (if (null? val) 'NIL (cdr val)))
         (pred (test-pred test))
         (tst-val (test-value test)))
    ;;(sformat ops-out "~a ~a ~a ~%" wme-val pred tst-val)
    (test-succeeds wme-val pred tst-val)))

(defun test-succeeds (x pred y)
  ;;(format "test-succeeds: ~a ~a ~a~%" x pred y)
  (let ((res (cond
               ((list? y) (letfuns ((find-success (val val-list)
                                                 (cond
                                                   ((null? val-list) ())
                                                   ((eql val (car val-list)) t) ; only pred allowed
                                                   (t (find-success val (cdr val-list))))))
                                  (find-success x y)))
               ((eql pred '<=>) (binary= (class-of x) (class-of y)))
               ((not (eql (class-of x) (class-of y))) ())
               ((eql pred '=)   (eql x y))
               ((eql pred '<>)  (not (eql x y)))
               ((eql pred '<)   (< x y))
               ((eql pred '>)   (> x y))
               ((eql pred '<=)  (<= x y))
               ((eql pred '>=)  (>= x y))
               (t (format "Error: Unknown predicate: ~a~%" pred)))))
    ;;(format "res: ~a~%" res)
    res))

(defmethod match-remove ((ce-manager <ce-manager>) wme cr-manager)
  ;;(print "match-remove" nl)
  ;;(print (wme-class-name wme) nl)
  ;;(print (member-alist (wme-class-name wme) (cond-els ce-manager)) nl)
  (let* ((class (wme-class-name wme))
         (ces (cadr (member-alist class (cond-els ce-manager)))))
    (do
     (lambda (x)
       ;;(print x nl)
       (match-remove-ce (wme-timestamp wme) x cr-manager))
     ces)))

(defmethod match-remove-ce (ts (ce <pos-join-ce>) cr-manager)
  (match-remove-join ts ce))

(defmethod match-remove-ce (ts (ce <neg-join-ce>) cr-manager)
  ;; join-tests are required by create-prod-insts
  (let ((join-tests (join-list ce ts)))
    (when (match-remove-join ts ce)
          (inform-create-prod-insts ts ce join-tests cr-manager))))

(defmethod match-remove-ce (ts (ce <pos-njoin-ce>) cr-manager)
  (match-remove-njoin ts ce))

(defmethod match-remove-ce (ts (ce <neg-njoin-ce>) cr-manager)
  (when (match-remove-njoin ts ce)
        (inform-create-prod-insts ts ce () cr-manager)))

(defun match-remove-njoin (ts ce)
  ;;(sformat ops-out "match-remove-njoin: ~a~%" ts)
  (let ((num-matches (ce-num-matched ce)))
    (letfuns ((rem-wme (ts match-list res)
                      (cond
                        ((null? match-list) res)
                        ((binary= (caar match-list) ts)
                         (set-ce-num-matched ce (- (ce-num-matched ce) 1))
                         (rem-wme ts (cdr match-list) res))
                        (t
                         (rem-wme ts (cdr match-list)
                                  (cons (car match-list) res))))))
            (set-ce-matches ce (rem-wme ts (ce-matches ce) ())))
    (< (ce-num-matched ce) num-matches)))

(defun match-remove-join (ts ce)
  (match-remove-njoin ts ce)
  (set-ce-jv-vals ce
                  (map
                   (lambda (x)
                     (if (not (null? (cdr x)))
                         (cons (car x)
                               (accumulate
                                (lambda (a y)
                                  (if (= (cdr y) ts)
                                      a
                                    (cons y a)))
                                ()
                                (cdr x)))
                       x))
                   (ce-jv-vals ce))))

;; inform-
;; Initiate appropriate join stage
(defun inform-create-prod-insts (ts ce join-tests cr-manager)
  ;;(print "inform-create-prod-insts" nl)
  (let ((prods (ce-prods ce)))
    (do
     (lambda (prod)
       (create-prod-insts prod ts ce join-tests cr-manager))
     prods)))

(defun inform-remove-prod-insts (ts ce cr-manager)
  (let ((prods (ce-prods ce)))
    (do
     (lambda (prod)
       (remove-prod-insts prod ts ce cr-manager))
     prods)))

;;;-----------------------------------------------------------------------------
;;; Join Process Stuff
;;;-----------------------------------------------------------------------------

;;;  is-satisfied
;;  Determine whether or not a condition element is satisfied
(defmethod is-satisfied ((ce <pos-join-ce>))
  (is-satisfied-pos ce))

(defmethod is-satisfied ((ce <neg-join-ce>))
  t ) ; can't determine anything

(defmethod is-satisfied ((ce <pos-njoin-ce>))
  (is-satisfied-pos ce))

(defmethod is-satisfied ((ce <neg-njoin-ce>))
  (= 0 (ce-num-matched ce)))

(defun is-satisfied-pos (ce)
  (> (ce-num-matched ce) 0))

;;;  join-list
;;  Get requested join list
(defun join-list (ce ts)
  (let* ((vals (member-alist ts (ce-matches ce)))
         (match (if (null? vals) () (caddr vals))))
    ;;(sformat ops-out "join-list: ~a~%" match)
    match))

;;; var-bindings
;;  Get requested variable bindings
(defun var-bindings (ce ts)
  (let* ((vals (member-alist ts (ce-matches ce)))
         (match (if (or (null? vals) (null? (cdr vals)))
                    () (cadr vals))))
    ;(sformat ops-out "~a ~a ~a ~%" ce ts match)
    match))

;;;-----------------------------------------------------------------------------
)  ;; End of module cond-el-2
;;;-----------------------------------------------------------------------------
