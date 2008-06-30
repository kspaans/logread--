;;; Takes a filename as the only commandline argument
(if (not (= 1 (vector-length (current-command-line-arguments))))
    (printf "Please give a filename as the only argument\n")
    (display (vector-ref (current-command-line-arguments) 0)))