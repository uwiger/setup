.PHONY: doc compile test compile_test clean_test run_test eunit clean

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

TESTDIRS= xtest/testapp-1 xtest/testapp-2

all: compile

compile:
	${REBAR3} compile

clean:
	${REBAR3} clean
	for D in $(TESTDIRS) ; do \
		(cd $$D; ${REBAR3} clean) ; \
	done

doc:
	${REBAR3} as doc do edoc

compile_test:
	for D in $(TESTDIRS) ; do \
		(cd $$D; ${REBAR3} compile) ; \
	done

clean_test:
	for D in $(TESTDIRS) ; do \
		(cd $$D; ${REBAR3} clean) ; \
	done
	rm -r xtest/releases

eunit: compile
	${REBAR3} eunit

test: eunit compile_test
	ERL_LIBS=${PWD}/_build/test/lib ./setup_gen test xtest/test.conf xtest/releases/1

run_test:
	erl -boot xtest/releases/1/start -config xtest/releases/1/sys

dialyzer:
	${REBAR3} dialyzer

ci: test dialyzer
	erl -boot xtest/releases/1/start -config xtest/releases/1/sys -s init stop
