NAME = uscheme
DEST = ../_build/default/${NAME}

default: compile

.PHONY: compile test clean

compile:
	dune build ${NAME}/${NAME}.bc --root=..
	cp ${DEST}/${NAME}.bc ./${NAME}
	chmod +w ./${NAME}

repl:
	utop -init ./utop_init

test:
	python3 run_tests.py

clean:
	rm -f ./${NAME}
	dune clean --root=..

