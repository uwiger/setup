.PHONY: doc compile test compile_test clean_test run_test

TESTDIRS= xtest/testapp-1 xtest/testapp-2

all: compile

compile:
	rebar compile

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
	rm -r xtest/releases

test: compile compile_test
	./setup_gen test xtest/test.conf xtest/releases/1

run_test:
	erl -boot xtest/releases/1/start -config xtest/releases/1/sys