CC := cs3110 compile -l nums

POSTFIX := ml

all: tests reals

reals: reals.ml
	$(CC) reals.$(POSTFIX)

tests: test_reals.ml
	$(CC) test_reals.$(POSTFIX)

test:
	cs3110 test test_reals.$(POSTFIX)
