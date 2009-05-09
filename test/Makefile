.PHONY: test clean

test:
	(. test.sh; tilde_and_compare hello)
	(. test.sh; tilde_and_compare no-initial-blank-lines)

clean:
	-rm *~
