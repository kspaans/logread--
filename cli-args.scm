;;; Takes a filename as the only commandline argument
(if (not (= 1 (vector-length (current-command-line-arguments))))
    (begin (printf "Please give a filename as the only argument\n") (exit))
    (void))
