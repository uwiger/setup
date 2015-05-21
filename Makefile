.PHONY: doc compile test compile_test clean_test run_test escriptize deps

REBAR ?= $(shell which rebar || echo ./rebar)

TESTDIRS= xtest/testapp-1 xtest/testapp-2

SETUP_PLT = setup.plt
DIALYZER_OPTS = # -Wunderspecs
DIALYZER_APPS = erts kernel stdlib sasl

all: compile

compile: deps
	${REBAR} compile

deps:
	${REBAR} get-deps

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

$(SETUP_PLT):
	rebar get-deps compile
	ERL_LIBS=deps dialyzer --build_plt --output_plt $(SETUP_PLT) \
	--apps $(DIALYZER_APPS)

clean_plt:
	rm -f $(SETUP_PLT)

dialyzer: deps compile $(SETUP_PLT)
	dialyzer -r ebin --plt $(SETUP_PLT) $(DIALYZER_OPTS)

ci: test dialyzer
	erl -boot xtest/releases/1/start -config xtest/releases/1/sys -s init stop
