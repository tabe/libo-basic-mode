#!/bin/bash

function tilde_and_compare() {
    emacs --batch -q -l ../ooo-basic-mode.el "$1.bas" --eval "(progn (ooo-basic-mode) (indent-region (point-min) (point-max)) (write-file \"$1.bas~\"))"
    diff -u $1.bas $1.bas~
}
