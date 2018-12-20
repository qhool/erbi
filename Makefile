.PHONY : all release test dialyze clean
APPLICATION=erbi

# rebar compile needs to occur twice to get the proper app file
all: rebar3
	@./rebar3 get-deps
	@./rebar3 compile

test: all
	@./rebar3 eunit skip_deps=true

dialyze: all ./.dialyzer.plt
	dialyzer \
	-pa ebin --plts ../dialyzer.plt ./.dialyzer.plt  \
	-Wno_return -Wno_unused \
	-I deps/ -I include/ --no_native --src -r src/

#project specific plt for dependencies
.dialyzer.plt: ../dialyzer.plt
	dialyzer --build_plt --output_plt ./.dialyzer.plt \
	--apps deps/epgsql deps/restc

#shared plt, which contains otp standard stuff
../dialyzer.plt:
	dialyzer --build_plt --output_plt ../dialyzer.plt \
           --apps erts kernel stdlib mnesia crypto public_key inets xmerl sasl \
                       eunit compiler debugger ssl tools

rebuild:

clean:
	@(test -f rebar3 && ./rebar3 clean) || exit 0
	@rm -rf deps || exit 0
	@rm -f rebar3 || exit 0
	@rm -rf .eunit || exit 0
	@rm -rf _build || exit 0

rebar3:
	@mkdir deps || exit 0
	@git clone https://github.com/erlang/rebar3.git deps/rebar3
	@(cd deps/rebar3; escript bootstrap; mv rebar3 ../..)

