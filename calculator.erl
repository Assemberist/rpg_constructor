-module(calculator).
-include("character.hrl").

-export([check_config/1]).

check_config(Objects) ->
	%% search duplicates
	uniq_objects(Objects),

	%% Complete class atributes of classes from parents
	Classes = [Obj || Obj = #class{} <- Objects],
	{Classes, _Loops} = search_parents(Classes),

	%% Get classes that have not founded parent
	{_NormalClasses, _NoParent} = lists:partition(
		fun	(#class{parents = []}) -> true;
			(_) -> false end,
		Classes),

	search_attributes(Objects).

search_attributes(Objects) ->
	Classes = [Obj || Obj = #class{} <- Objects],
	States = [Obj#stat.name || Obj = #stat{} <- Objects],
	Propertyes = [Obj#parameter.name || Obj = #parameter{} <- Objects],
	Actions = [Obj#action.name || Obj = #action{} <- Objects],

	ClassStats = lists:usort([Stat || #class{stats = Stats} <- Classes, Stat <- Stats]),
	ClassPropertyes = lists:usort([Stat || #class{parameters = Stats} <- Classes, Stat <- Stats]),
	ClassActions = lists:usort([Stat || #class{actions = Stats} <- Classes, Stat <- Stats]),

	NotFounded = #class{
		stats = ClassStats -- States,
		parameters = ClassPropertyes -- Propertyes,
		actions = ClassActions -- Actions},

	NotUsed = #class{
		stats = States -- ClassStats,
		parameters = Propertyes -- ClassPropertyes,
		actions = Actions -- ClassActions},

	{NotFounded, NotUsed}.
	
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
	{[get(Class#class.name) || Class <- Classes], get(inherit_loops)}.

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
			put(inherit_loops, [[Class#class.name | Stack] | get(inherit_loops)])
	end.

merge_classes(Child, Parent) ->
	Child#class{
		parents = Child#class.parents -- [Parent#class.name],
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
