.PHONY: test

test:
	emacs --batch -l ooo-basic-mode.el -l test.el
