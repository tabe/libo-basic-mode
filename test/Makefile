.PHONY: test clean

test:
	(. test.sh; tilde_and_compare hello)
	(. test.sh; tilde_and_compare multiline-if)
	(. test.sh; tilde_and_compare no-initial-blank-lines)
	(. test.sh; new_definition new-empty new-sub)
	(. test.sh; new_definition new-sub new-function)
	(. test.sh; new_definition new-function new-type)
	(. test.sh; new_definition new-type new-empty)

clean:
	-rm *~
