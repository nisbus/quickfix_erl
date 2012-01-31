all: deps
	@rebar compile

deps:
	@rebar get-deps update-deps

compile:
	@rebar compile

generate: clean deps compile
	@rebar generate

clean:
	rm -rf rel/quickfix_erl

test: compile
	@rebar eunit skip_deps=true
	chromium .eunit/index.html &

shell: all
	erl -pa /home/nisbus/code/erlang/quickfix_erl/deps/erlsom/ebin /home/nisbus/code/erlang/quickfix_erl/deps/jsx/ebin

run: generate
	./rel/quickfix_erl/bin/quickfix_erl console
