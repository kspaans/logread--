(require (lib "etc.ss"))
(require (lib "list.ss"))
(require (lib "time.ss" "srfi" "19"))

;; Here is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.

;; scan: (list char) trans-table symbol (list symbol) -> (list token)
(define (scan str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.
(define-struct token (kind lexeme) #f)

;; A token is a (make-token k l), where k is a symbol
;;  and l is a (list char).
(define-struct transition (state charset next) #f)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (asmtrlst, 'start, asmfinal).
;; Definitions of asmtrlst and asmfinal follow.

;; functions used in defining sample transition table
(define (chartest ch)
  (lambda (x) (char=? x ch)))
(define (char-not-whitespace? x)
  (not (char-whitespace? x)))

;; sample transition table
(define asmtrlst 
  (list
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start char-not-whitespace? 'word)
   (make-transition 'word char-not-whitespace? 'word)
   ))

;; sample list of final states
(define asmfinal
  (list
    'word
    'whitespace
    ))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (list char) trans-table symbol (listof symbol) (list char) (list token) -> (list token)
(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl) 
       (if (member state final)
           (reverse (cons (finalize-token state (reverse acc)) tacc))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'whitespace)
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl) 
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc
(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

(define (finalize-token state l)
  (make-token state l))

;; Some very basic tests
(define (testscan str)
  (scan str asmtrlst 'start asmfinal))

;; token-pass2 : (listof tokens) -> (listof tokens)
;; Takes a list of tokens and does a second pass on them. It will context-
;; sensitively translate some tokens into dates, hosts, etc.
(define (token-pass2 l)
  (token-pass2-h l 3 empty))
;; token-pass2-h : (listof tokens) int (listof char) -> (listof tokens)
;; Helper function for token-pass2, it collects the lexemes of the first 3
;; tokens and stuffs them into a single new token, ready to be stuffed into the
;; SRFI-19 date struct.
(define (token-pass2-h lst cnt acc)
  (cond
    ((empty? lst) (cons (make-token 'date (cdr acc)) lst)) ; FIXMEUGLY
    ((zero? cnt) (cons (make-token 'date (cdr acc)) lst))  ; FIXMEUGLY
    (else (token-pass2-h (cdr lst)
                         (sub1 cnt)
                         (append acc '(#\space) (token-lexeme (car lst))))))) ; can this be done better? FIXMEUGLY
;; token-pass3 : (listof tokens) (listof string) => (listof tokens)
;; Passes it's input (from test-scan) through token-pass2 first, then goes over
;; that to find the host token. If the host doesn't match something from the
;; ``known hosts list'' then it will keep it as a plain 'word token. Or maybe
;; make it an 'unsure token? This can be useful latey, to flag new log file
;; lines that aren't recognized.
(define (token-pass3 l hosts)
  (let ((pass2 (token-pass2 l)))
    (if (equal? 'date (token-kind (car pass2)))
        (if (member (list->string (token-lexeme (cadr pass2))) hosts)
            (cons (car pass2)
                  (cons (make-token 'host (token-lexeme (cadr pass2)))
                        (caddr pass2)))
            pass2)
        pass2)))
                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/Desktop/testeez.scm")
(define-syntax %logtokenize:testeez
  (syntax-rules ()
    ((_ X ...)
     ;; Note: Comment-out exactly one of the following two lines.
     ;; (error "Tests disabled.")
     (testeez X ...))))
(define (%logtokenize:test)
  (%logtokenize:testeez
   "Tokenize log file lines into words, numbers, dates, etc..."
   ;(test/equal "Put two and two together" (+ 2 2) 4)
   (test/equal "Sample log file line, should be all words"
               (map token-kind (testscan "Jun 29 20:52:11 large bootpd[610]: DHCP REQUEST [en0]: 1,0:d0:59:82:b1:a4 <jeremy>"))
               '(word word word word word word word word word word))
   (test/equal "Tokenize pass two: turn a single token into a date"
               (map token-kind (token-pass2 (testscan "03/07/2008")))
               '(date))
   (test/equal "Tokenize pass two: turn two tokens into a date"
               (map token-kind (token-pass2 (testscan "July 2008")))
               '(date))
   (test/equal "Tokenize pass two: turn 3 tokens into a date"
               (map token-kind (token-pass2 (testscan "July 3 2008")))
               '(date))
   (test/equal "Tokenize pass two: make first three the date"
               (map token-kind (token-pass2 (testscan "Jun 29 21:18:48 large configd[71]: AppleTalk startup")))
               '(date word word word word))
   (test/equal "Tok-Pass-2: cat the first three tokens into a single string - ready to conver to SRFI-19 date struct"
               (list->string
                (token-lexeme
                 (car
                  (token-pass2
                   (testscan
                    "Jun 29 21:18:48 large configd[71]: AppleTalk )startup")))))
               "Jun 29 21:18:48")
   (test/equal "Tok-Pass-2: make the 4th word into a host token!"
               (map token-kind (token-pass3 (testscan "Jun 29 21:18:48 large foo") '("large" "localhost")))
               '(date host))
   ))

(%logtokenize:test)
;;;;;; need more tests to turn the second word, if matching large|localhost into a kind=host token
    

  ; (printf "~n~a~n" (token-lexeme (car (token-pass2 (testscan (car all-lines))))))
  ; (string->date " Jun 29 20:30:18" "~b ~d ~H:~M:~S")
