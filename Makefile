.PHONY: test clean

test:
	emacs --batch -q -l ooo-basic-mode.el -l test.el
	$(MAKE) -C test test

clean:
	$(MAKE) -C test clean
