.PHONY: doc

all: compile

compile:
	rebar compile escriptize

doc:
	rebar doc
