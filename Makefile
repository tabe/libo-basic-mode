.PHONY: test clean

test:
	emacs --batch -q -l libo-basic-mode.el -l test.el
	$(MAKE) -C test test

check: test clean

clean:
	$(MAKE) -C test clean
