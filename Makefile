.PHONY: all clean eunit ebin test deps cleandeps wipe

all: deps ebin

deps:
	@sh ./get-rabbit-client.sh
	@./rebar get-deps

ebin:
	@./rebar compile

test: eunit

eunit:
	@./rebar skip_deps=true eunit

clean:
	@./rebar clean

cleandeps:
	@./rebar delete-deps

wipe: clean cleandeps
	@rm *.dump
