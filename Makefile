all:
	@rebar compile
shell: all
	erl -pa /home/nisbus/code/erlang/quickfix_erl/deps/erlsom/ebin /home/nisbus/code/erlang/quickfix_erl/deps/jsx/ebin