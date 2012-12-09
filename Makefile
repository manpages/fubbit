.PHONY: all clean eunit ebin test deps cleandeps wipe erl

all: deps ebin

deps:
	sh ./get-rabbit-client.sh
	./rebar -C .rebar.config get-deps

ebin:
	./rebar -C .rebar.config compile

test: eunit

eunit:
	@./rebar -C .rebar.config skip_deps=true eunit

clean:
	@./rebar -C .rebar.config clean

cleandeps:
	@./rebar -C .rebar.config delete-deps

wipe: clean cleandeps
	@rm *.dump || true
	@rm -rf ./deps/*
	@rm -rf ./amqp_client
	@rm -rf ./rabbit_common

sasl: all
	erl -pa ./ebin/ ./deps/*/ebin ./amqp_client/ebin/ ./rabbit_common/ebin/ -boot start_sasl -s fubbit_app

erl: all
	erl -pa ./ebin/ ./deps/*/ebin ./amqp_client/ebin/ ./rabbit_common/ebin/ -s fubbit_app
