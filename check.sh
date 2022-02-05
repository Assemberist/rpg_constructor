#!/usr/bin/env escript

main([Arg | _]) ->
	{ok, Terms} = file:consult(Arg),
	io:format("~w\n",	[calculator:check_config(Terms)]).

