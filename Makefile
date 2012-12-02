fubbit_src := $(wildcard src/*.erl)

.PHONY: all clean eunit test deps cleandeps wipe

all: deps ebin

deps:
	rebar get-deps

ebin: $(fubbit_src)
	rebar compile

test: eunit

eunit:
	./rebar skip_deps=true eunit

clean:
	./rebar clean

cleandeps:
	./rebar delete-deps

wipe: clean cleandeps
	@rm log/*
	@rm *.dump
