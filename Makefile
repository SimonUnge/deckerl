REBAR=`which rebar || ./rebar`

all: compile

compile:
	@$(REBAR) compile

tests: eunit commontest

eunit:
	@$(REBAR) skip_deps=true eunit

commontest:
	@$(REBAR) ct

dialyze:
	@dialyzer --src src/*.erl test/*.erl

clean:
	@$(REBAR) clean
	@rm -r ebin/ || true
	@rm -r logs/ || true
	@rm ct/*.beam || true
