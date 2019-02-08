export PATH=$PATH:~/rebar3/
rm -r _build/*
rm -r Mnesia*
rebar3 compile
rebar3 shell
