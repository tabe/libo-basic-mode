#!/bin/sh

tilde_and_compare() {
    emacs --batch -q -l ../ooo-basic-mode.el "$1.bas" --eval "(progn (ooo-basic-mode) (indent-region (point-min) (point-max)) (write-file \"$1.bas~\"))"
    diff -u $1.bas $1.bas~
}

new_definition() {
    emacs --batch -q -l ../ooo-basic-mode.el "$1.bas" --eval "(progn (ooo-basic-new-definition) (write-file \"$2.bas~\"))"
    diff -u $2.bas $2.bas~
}
