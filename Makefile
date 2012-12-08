.PHONY: all clean eunit ebin test deps cleandeps wipe erl

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
	@rm *.dump || true
	@rm -rf ./deps/*
	@rm -rf ./amqp_client
	@rm -rf ./rabbit_common

erl: all
	erl -pa ./ebin/ ./deps/*/ebin ./amqp_client/ebin/ ./rabbit_common/ebin/ -boot start_sasl -s fubbit_app
