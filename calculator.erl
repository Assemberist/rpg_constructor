-module(calculator).
-include("character.hrl").

-export([check_config/1]).

check_config(Objects) ->
	%% search duplicates
	uniq_objects(Objects),

	Classes = [Obj || Obj = #class{} <- Objects],
	search_parents(Classes).
	
search_parents(Classes) ->
	put(inherit_loops, []),
	%% store Classes in the dictionary
	[put(Class#class.name, Class) || Class <- Classes],

	{Base, _} = lists:partition(
		fun (#class{parents = []}) -> true;
			(_) -> false end,
		Classes),

	lists:map(fun(X)-> merge_parent_info(X, Classes, []) end, Base),

	%% upload updated clases from dictionary
	[get(Class#class.name) || Class <- Classes].

merge_parent_info(Class, AllClasses, Stack) ->
	case lists:member(Class#class.name, Stack) of
		false ->
			Childs = [Ch || Ch = #class{parents = Par} <- AllClasses, lists:member(Class#class.name, Par)],
			lists:foreach(
				fun(X) ->
					UpdateChild = merge_classes(get(X#class.name), Class),
					put(X#class.name, UpdateChild),
					merge_parent_info(X, AllClasses, [Class#class.name | Stack])
				end,
				Childs);

		true ->
			[put(inherit_loops, [[Class#class.name | Stack] | get(inherit_loops)])]
	end.

merge_classes(Child, Parent) ->
	Child#class{
		actions = Parent#class.actions ++ Child#class.actions, 
		stats = Parent#class.stats ++ Child#class.stats, 
		parameters = Parent#class.parameters ++ Child#class.parameters
	}.

uniq_objects(Objects) ->
	Class_names = [Obj#class.name || Obj = #class{} <- Objects],
	Stat_names = [Obj#stat.name || Obj = #stat{} <- Objects],
	Property_names = [Obj#parameter.name || Obj = #parameter{} <- Objects],
	Action_names = [Obj#action.name || Obj = #action{} <- Objects],

	search_duplicates(Class_names, []) ++
	search_duplicates(Stat_names, []) ++
	search_duplicates(Property_names, []) ++
	search_duplicates(Action_names, []).
	
search_duplicates([], Acc) ->
	Acc;	

search_duplicates([Head | Tail], Acc) ->
	NewAcc = case lists:member(Head, Tail) of
		true -> [Head | Acc];
		false -> Acc
	end,
	search_duplicates(Tail, NewAcc).
