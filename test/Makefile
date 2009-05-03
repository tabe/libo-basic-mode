.PHONY: test clean

test:
	emacs --batch -q -l ../ooo-basic-mode.el hello.bas --eval '(progn (ooo-basic-mode) (indent-region (point-min) (point-max)) (write-file "hello.bas~"))'
	diff -u hello.bas hello.bas~

clean:
	-rm *~
