#!/usr/bin/env escript

main(_)->
	{ok, Terms} = file:consult("term.txt"),
	io:format("~p\n",	[calculator:check_config(Terms)]).

