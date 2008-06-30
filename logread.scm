;;; Kyle Spaans - July 2008
;;; LOG READER
;;; The reader of logfiles, just like Don and I worked on in the Dojo last week.
;;; I'll aim this at system type logfiles.
;;; Takes a single argument, should be a filename.
;;; It'll read that file and parse it and output accordingly.
;;;
;;; I'm going to start by using the syslog logs on my mactop, large. First I
;;; need to figure out what kind of information I want out of these logs. They
;;; all seem to have the same basic format.
;;;  date host message
;;;  - The date I can parse and structurize using SRFI 19:
;;;     (string->date "Jun 29 20:30:18" "~b ~d ~H:~M:~S")
;;;  - The hostname always is either large or localhost, easy to tokenize
;;;  - The message is the fun part! I can start by looking for and tokenizing
;;;    specific message forms, like "daemon[pid]: bob loblaw ..." and
;;;    "sudo: user: cmd ..."
;;; Therefore, I can use my tokenizer code from CS241 in the Winter, and adapt
;;; it to tokenize each logfile line into '(date host message), where the date
;;; token can hold an SRFI #<struct:tm:date>, host can hold just a string, and
;;; message is the trickier part! Should it have kinds for each message type
;;; like: daemon, sudo, unsure, ... - each containing a string (or perhaps a
;;; list of tokens/words), _OR_ should the message part just be a list of tokens
;;; such that the tokens are more specialized to indicate the message type.
;;;   I'll try the former for now.

(require (lib "time.ss" "srfi" "19"))

;===============================================================================
;===============================================================================
;; check the commandline arguments!
(load "cli-args.scm");;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;===============================================================================
;; load the tokenizer code
(load "scanner.scm");;;;;;
;===============================================================================


(define-struct logline
  (date    ; an SRFI-19 struct representing the date
   host    ; a single word token => (listof char)
   message ; a list of word tokens => (listof char)
))


(define FILENAME (open-input-file (vector-ref (current-command-line-arguments) 0)))

;(eof-object? (read-line))...
;(let ((linestring (read-line FILENAME)))
;  (define DATE (substring linestring 0 15))
;  (define HOST (substring linestring 16 21))
;  (printf "The first line's date is~n~s~nand the host is~n~s~n" DATE HOST))

;; Recursive function that reads all lines in the file and returns a list of
;; them. Will also delimit the syslog line nicely
(define (read-all-lines)
  (let ((lines (read-line FILENAME)))
    (cond
      ((eof-object? lines) '())
      (#t (cons lines (read-all-lines))))))
(define all-lines (read-all-lines))
(close-input-port FILENAME) ; don't need it open any longer

;===============================================================================

;;;; need a function to turn tokens into the struct for each line
(map (lambda (x)
       (printf "~a~n" x))
     (map token-kind (token-pass2 (testscan (car all-lines)))))
