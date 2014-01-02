.PHONY : all release test dialyze clean
APPLICATION=erbi

# rebar compile needs to occur twice to get the proper app file
all: rebar
	@./rebar get-deps
	@./rebar compile

test: all
	@./rebar eunit skip_deps=true

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
	@(test -f rebar && ./rebar clean) || exit 0
	@rm -rf deps || exit 0
	@rm -f rebar || exit 0
	@rm -rf .eunit || exit 0

rebar:
	@mkdir deps || exit 0
	@git clone git@github.com:basho/rebar.git deps/rebar
	@(cd deps/rebar; escript bootstrap; mv rebar ../..)

