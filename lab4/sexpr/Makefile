#
# Makefile for sexpr library.
#

BUILD = ../_build/default/sexpr
LIB   = sexprlib.cma

compile:
	dune build ${LIB}

test:
	dune build lexer_test.bc
	cp ${BUILD}/lexer_test.bc lexer_test
	dune build parser_test.bc
	cp ${BUILD}/parser_test.bc parser_test
	dune build repl_test.bc
	cp ${BUILD}/repl_test.bc repl_test

clean:
	dune clean
	rm -f lexer_test parser_test repl_test

