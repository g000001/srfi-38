;;;; srfi-38.lisp

(cl:in-package :srfi-38.internal)

(defun char-whitespace? (c)
  (and (member c '(#\Space #\Newline #\Tab #\Newline #\Return #\Page))
       T))

(defun write (obj port)
  (cl:write obj :stream port))

;;; A printer that shows all sharing of substructures.  Uses the Common
;;; Lisp print-circle notation: #n# refers to a previous substructure
;;; labeled with #n=.   Takes O(n^2) time.

;; We only track duplicates of pairs, vectors, and strings.  We
;; ignore zero-length vectors and strings because r5rs doesn't
;; guarantee that eq? treats them sanely (and they aren't very
;; interesting anyway).
(define-function (interesting? obj)
  (or (pair? obj)
      (and (vector? obj) (not (zero? (vector-length obj))))
      (and (string? obj) (not (zero? (string-length obj)))) ))

(define-function (write-interesting alist
                                    &optional obj (outport *standard-output*))
  (typecase obj
    (cons
     (display "(" outport)
     (srfi-5:let write-cdr ((obj (cdr obj)) (alist (write-obj (car obj) alist)))
       (cond ((and (pair? obj) (not (cdr (assq obj alist))))
              (display " " outport)
              (write-cdr (cdr obj) (write-obj (car obj) alist)) )
             ((null? obj)
              (display ")" outport)
              alist )
             (:else
              (display " . " outport)
              (let ((alist (write-obj obj alist)))
                (display ")" outport)
                alist )))))
    (vector
     (display "#(" outport)
     (let ((len (vector-length obj)))
       (srfi-5:let write-vec ((i 1) (alist (write-obj (vector-ref obj 0) alist)))
         (cond ((= i len) (display ")" outport) alist)
               (:else (display " " outport)
                      (write-vec (+ i 1)
                                 (write-obj (vector-ref obj i) alist) ))))))
    (T (write obj outport) alist) ))

;; (write-obj OBJ ALIST):
;; ALIST has an entry for each interesting part of OBJ.  The
;; associated value will be:
;;  -- a number if the part has been given one,
;;  -- #t if the part will need to be assigned a number but has not been yet,
;;  -- #f if the part will not need a number.
;; The cdr of ALIST's first element should be the most recently
;; assigned number.
;; Returns an alist with new shadowing entries for any parts that
;; had numbers assigned.
(define-function (write-obj obj alist &optional (outport *standard-output*))
  (cond ((interesting? obj)
         (let ((val (cdr (assq obj alist))))
           (cond ((not val) (write-interesting alist obj outport))
                 ((numberp val)
                  (begin (display "#" outport)
                         (write val outport)
                         (display "#" outport) alist))
                 (:else
                  (let ((n (+ 1 (cdar alist))))
                    (begin (display "#" outport)
                           (write n outport)
                           (display "=" outport) )
                    (write-interesting (acons obj n alist) obj outport) )))))
        (:else (write obj outport) alist) ))

;; Scan computes the initial value of the alist, which maps each
;; interesting part of the object to #t if it occurs multiple times,
;; #f if only once.
(define-function (scan obj alist)
  (srfi-61:cond
    ((not (interesting? obj)) alist)
    ((assq obj alist)
     :=> (lambda (p) (if (cdr p) alist (acons obj 'T alist))))
    (:else
     (let ((alist (acons obj 'NIL alist)))
       (cond ((pair? obj) (scan (car obj) (scan (cdr obj) alist)))
             ((vector? obj)
              (let ((len (vector-length obj)))
                (do ((i 0 (+ 1 i))
                     (alist alist (scan (vector-ref obj i) alist)) )
                    ((= i len) alist) )))
             (:else alist) )))))

(define-function (write-with-shared-structure obj . optional-port)
  (let ((outport (if (eq? '() optional-port)
                     *standard-output*
                     (car optional-port) )))
    (write-obj obj (acons 'dummy 0 (scan obj '())) outport)
    ;; We don't want to return the big alist that write-obj just returned.
    (values)))

(define-function char-standard-case
  (if (char=? #\a (char (symbol->string 'a) 0))
      #'char-downcase
      #'char-upcase ))

(define-function (read-with-shared-structure . optional-port)
  (let ((port
         (if (null? optional-port) *standard-input* (car optional-port)) )
        ;; Parts-alist maps the number of each part to a thunk
        ;; that returns the part.
        (parts-alist '()) )
    (with-local-define-function
      (define-function (read-char* port) (read-char port nil +eof+))
      (define-function (peek-char*  port) (peek-char nil port nil +eof+))
      (define-function (looking-at? c port)
        (eqv? c (peek-char* port)) )
      (define-function (delimiter? c)
        (case c
          ((#\( #\) #\" #\;) 'T)
          (otherwise
           (or (eof-object? c)
               (char-whitespace? c) ))))
      (define-function (not-delimiter? c) (not (delimiter? c)))
      (define-function (eat-intertoken-space port)
        (let ((c (peek-char* port)))
          (cond ((eof-object? c))
                ((char-whitespace? c) (read-char* port) (eat-intertoken-space port))
                ((char=? c #\;)
                 (do ((c (read-char* port) (read-char* port)))
                     ((or (eof-object? c) (char=? c #\newline))) )
                 (eat-intertoken-space port) ))))
      (define-function (read-string port)
        (read-char* port)
        (srfi-5:let read-it ((chars '()))
                    (let ((c (read-char* port)))
                      (if (eof-object? c)
                          (error "EOF inside a string")
                          (case c
                            ((#\") (list->string (reverse chars)))
                            ((#\\) (read-it (cons (read-char* port) chars)))
                            (otherwise (read-it (cons c chars))) )))))
      ;; reads chars that match PRED and returns them as a string.
      (define-function (read-some-chars pred port)
        (srfi-5:let iter ((chars '()))
                    (let ((c (peek-char* port)))
                      (if (or (eof-object? c) (not (funcall pred c)))
                          (list->string (reverse chars))
                          (iter (cons (read-char* port) chars)) ))))
      ;; reads a character after the #\ has been read.
      (define-function (read-character port)
        (let ((c (peek-char* port)))
          (cond ((eof-object? c) (error "EOF inside a character"))
                ((char-alphabetic? c)
                 (let ((name (read-some-chars #'char-alphabetic? port)))
                   (cond ((= 1 (string-length name)) (string-ref name 0))
                         ((string-ci=? name "space") #\space)
                         ((string-ci=? name "newline") #\newline)
                         (:else (error "Unknown named character: " name)) )))
                (:else (read-char* port)) )))
      (define-function (string-standard-case str)
        (let* ((len (string-length str))
               (new (make-string len)) )
          (do ((i 0 (+ i 1)))
              ((= i len) new)
            (string-set! new i (char-standard-case (string-ref str i))) )))

      (define-function (read-number first-char port)
        (let ((str (string-append (string first-char)
                                  (read-some-chars #'not-delimiter? port) )))
          (or (string->number str)
              (error "Malformed number: " str) )))

      (define-function (add-part-to-alist! n thunk)
        (set! parts-alist (cons (cons n thunk) parts-alist)) )

      (define-function (read-identifier port)
        (string->symbol
         (string-standard-case
          (read-some-chars #'not-delimiter? port) )) )

      (define-function (read-part-spec port)
        (let ((n (string->number (read-some-chars #'char-numeric? port))))
          (let ((c (read-char* port)))
            (case c
              ((#\=) (cons 'decl n))
              ((#\#) (cons 'use n))
              (otherwise (error "Malformed shared part specifier")) ))))

      ;; Tokens: strings, characters, numbers, booleans, and
      ;; identifiers/symbols are represented as themselves.
      ;; Single-character tokens are represented as (CHAR), the
      ;; two-character tokens #( and ,@ become (#\#) and (#\@).
      ;; #NN= and #NN# become (decl . NN) and (use . NN).
      (define-function (read-optional-token port)
        (eat-intertoken-space port)
        (let ((c (peek-char* port)))
          (case c
            ((#\( #\) #\' #\`) (read-char* port) (list c))
            ((#\,)
             (read-char* port)
             (if (looking-at? #\@ port)
                 (begin (read-char* port) '(#\@))
                 '(#\,) ))
            ((#\") (read-string port))
            ((#\.)
             (read-char* port)
             (cond ((delimiter? (peek-char* port)) '(#\.))
                   ((not (looking-at? #\. port)) (read-number #\. port))
                   ((begin
                      (read-char* port)
                      (looking-at? #\. port) )
                    (read-char* port)
                    '|...| )
                   (:else (error "Malformed token starting with \"..\"")) ))
            ((#\+) (read-char* port) (if (delimiter? (peek-char* port))
                                         '+ (read-number c port)))
            ((#\-) (read-char* port)
             (if (delimiter? (peek-char* port)) '- (read-number c port)) )
            ((#\#)
             (read-char* port)
             (let ((c (peek-char* port)))
               (case c
                 ((#\() (read-char* port) '(#\#))
                 ((#\\) (read-char* port) (read-character port))
                 ((#\t #\T) (read-char* port) 'T)
                 ((#\f #\F) (read-char* port) 'NIL)
                 (otherwise
                  (cond ((eof-object? c)
                         (error "EOF inside a # token") )
                        ((char-numeric? c) (read-part-spec port))
                        (:else (read-number #\# port)) )))))
            (otherwise
             (cond ((eof-object? c) c)
                   ((char-numeric? c)
                    (read-char* port)
                    (read-number c port) )
                   (:else (read-identifier port)) )))))


      (define-function (read-token port)
        (let ((tok (read-optional-token port)))
          (if (eof-object? tok)
              (error "EOF where token was required")
              tok )))

      ;; Read-object returns a datum that may contain some thunks, which
      ;; need to be replaced with their return values.
      (define-function (read-object port)
        (finish-reading-object (read-token port) port) )

      ;; Like read-object, but may return EOF.
      (define-function (read-optional-object port)
        (finish-reading-object (read-optional-token port) port) )

      (define-function (finish-reading-object first-token port)
        (if (not (pair? first-token))
            first-token
            (if (char? (car first-token))
                (case (car first-token)
                  ((#\() (read-list-tail port))
                  ((#\#) (list->vector (read-list-tail port)))
                  ((#\. #\)) (error
                              (string-append "Unexpected \""
                                             (string (car first-token))
                                             "\"" )))
                  (otherwise
                   (list (caadr (assv (car first-token)
                                      '((#\' 'x) (#\, (unquote x)) (#\` `x) (#\@ (unquote-splice x))) ))
                         (read-object port) )))
                ;; We need to specially handle chains of declarations in
                ;; order to allow #1=#2=x and #1=(#2=#1#) and not to allow
                ;; #1=#2=#1# nor #1=#2=#1=x.
                (let ((starting-alist parts-alist))
                  (srfi-5:let
                      read-decls ((token first-token))
                      (if (and (pair? token) (symbol? (car token)))
                          (let ((n (cdr token)))
                            (case (car token)
                              ((use)
                               ;; To use a part, it must have been
                               ;; declared before this chain started.
                               (srfi-61:cond
                                 ((cl:assoc n starting-alist) :=> #'cdr)
                                 (:else (error "Use of undeclared part " n)) ))
                              ((decl)
                               (if (cl:assoc n parts-alist :test #'=)
                                   (error "Double declaration of part " n) )
                               ;; Letrec enables us to make deferred
                               ;; references to an object before it exists.
                               (letrec ((obj (begin
                                               (add-part-to-alist! n (lambda () obj))
                                               (read-decls (read-token port)) )))
                                 obj ))))
                          (finish-reading-object token port) ))))))

      (define-function (read-list-tail port)
        (let ((token (read-token port)))
          (if (not (pair? token))
              (cons token (read-list-tail port))
              (case (car token)
                ((#\)) '())
                ((#\.) (let* ((obj (read-object port))
                              (tok (read-token port)) )
                         (if (and (pair? tok) (char=? #\) (car tok)))
                             obj
                             (error "Extra junk after a dot") )))
                (otherwise
                 (let ((obj (finish-reading-object token port)))
                   (cons obj (read-list-tail port)) ))))))

      ;; Unthunk.
      ;; To deference a part that was declared using another part,
      ;; e.g. #2=#1#, may require multiple dethunkings.  We were careful
      ;; in finish-reading-object to ensure that this won't loop forever:
      (define-function (unthunk thunk)
        (let ((x (funcall thunk)))
          (if (procedure? x) (unthunk x) x) ))
      :in
      (let ((obj (read-optional-object port)))
        (srfi-5:let fill-in-parts ((obj obj))
                    (cond ((pair? obj)
                           (if (procedure? (car obj))
                               (set-car! obj (unthunk (car obj)))
                               (fill-in-parts (car obj)) )
                           (if (procedure? (cdr obj))
                               (set-cdr! obj (unthunk (cdr obj)))
                               (fill-in-parts (cdr obj)) ))
                          ((vector? obj)
                           (let ((len (vector-length obj)))
                             (do ((i 0 (+ i 1)))
                                 ((= i len))
                               (let ((elt (vector-ref obj i)))
                                 (if (procedure? elt)
                                     (vector-set! obj i (unthunk elt))
                                     (fill-in-parts elt) )))))))
        obj ))))
