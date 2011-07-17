#!/bin/sh

tilde_and_compare() {
    emacs --batch -q -l ../libo-basic-mode.el "$1.bas" --eval "(progn (libo-basic-mode) (indent-region (point-min) (point-max)) (write-file \"$1.bas~\"))"
    diff -u $1.bas $1.bas~
}

new_definition() {
    emacs --batch -q -l ../libo-basic-mode.el "$1.bas" --eval "(progn (libo-basic-new-definition) (write-file \"$2.bas~\"))"
    diff -u $2.bas $2.bas~
}
