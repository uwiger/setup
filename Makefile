.PHONY: doc compile test compile_test clean_test run_test

TESTDIRS= test/testapp-1 test/testapp-2

all: compile

compile:
	rebar compile escriptize

doc:
	rebar doc

compile_test:
	for D in $(TESTDIRS) ; do \
	(cd $$D; rebar compile) ; \
	done

clean_test:
	for D in $(TESTDIRS) ; do \
	(cd $$D; rebar clean) ; \
	done
	rm -r test/releases

test: compile compile_test
	./setup_gen test test/test.conf test/releases/1

run_test:
	erl -boot test/releases/1/start -config test/releases/1/sys