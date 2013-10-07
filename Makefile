.PHONY : all release test dialyze clean
APPLICATION=erbi

# rebar compile needs to occur twice to get the proper app file
all: rebar
	@./rebar get-deps
	@./rebar compile

test: all
	@./rebar eunit skip_deps=true

dialyze: all ../dialyzer.plt
	dialyzer --plt ../dialyzer.plt \
           -Wno_return -Wno_unused --src \
           src/

../dialyzer.plt:
	dialyzer --build_plt --output_plt ../dialyzer.plt \
           --apps erts kernel stdlib mnesia crypto inets xmerl sasl \
                       compiler debugger ssl tools

clean:
	@(test -f rebar && ./rebar clean) || exit 0
	@rm -rf deps || exit 0
	@rm -f rebar || exit 0
	@rm -rf .eunit || exit 0

rebar:
	@mkdir deps || exit 0
	@git clone git@github.com:basho/rebar.git deps/rebar
	@(cd deps/rebar; escript bootstrap; mv rebar ../..)

