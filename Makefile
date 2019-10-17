compile:
	rebar3 compile

all_checks:
	rebar3 xref
	rebar3 compile
	rebar3 gradualizer
	rebar3 dialyzer
