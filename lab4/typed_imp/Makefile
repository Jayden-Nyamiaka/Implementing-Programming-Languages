NAME = typed_imp
DEST = ../_build/default/${NAME}

default: clean compile

.PHONY: compile test clean

compile:
	dune build ${NAME}/${NAME}.bc --root=..
	cp ${DEST}/${NAME}.bc ./${NAME}
	chmod +w ./${NAME}

test:
	python3 run_tests.py

clean:
	rm -f ./${NAME}
	dune clean --root=..

