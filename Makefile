.PHONY : all release test dialyze clean
APPLICATION=db_objects
TEST_SUITES=voalte_ref_data_tests,voalte_user_tests,voalte_client_tests

# rebar compile needs to occur twice to get the proper app file
all: rebar
	@./rebar get-deps
	@./rebar compile

release: all
	@rm -rf rel/$(APPLICATION) || exit 0
	@deps/reltool_util/release

test: all
	@./rebar ct suites=$(TEST_SUITES) skip_deps=true

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
	@rm -rf rel/$(APPLICATION) || exit 0
	@rm -rf deps || exit 0
	@rm -f rebar || exit 0
	@rm -rf test/logs || exit 0
	@rm -rf test/*.beam || exit 0

rebar:
	@mkdir deps || exit 0
	@git clone -b voalte-master git@code.voalte.net:voalte/rebar.git \
                              deps/rebar
	@(cd deps/rebar; escript bootstrap; mv rebar ../..)

