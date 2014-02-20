.PHONY: doc compile test compile_test clean_test run_test escriptize

REBAR ?= $(shell which rebar || echo ./rebar)

TESTDIRS= xtest/testapp-1 xtest/testapp-2

all: compile

compile:
	${REBAR} compile

doc:
	${REBAR} doc

compile_test:
	for D in $(TESTDIRS) ; do \
	(cd $$D; ${REBAR} compile) ; \
	done

clean_test:
	for D in $(TESTDIRS) ; do \
	(cd $$D; ${REBAR} clean) ; \
	done
	rm -r xtest/releases

test: compile compile_test
	./setup_gen test xtest/test.conf xtest/releases/1

run_test:
	erl -boot xtest/releases/1/start -config xtest/releases/1/sys

escriptize:
	${REBAR} skip_deps=true escriptize
