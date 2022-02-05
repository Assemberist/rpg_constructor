-record (stat, {
	name		= ""	:: string(),
	description	= ""	:: string()}).

-record (action, {
	name		= ""	:: string(),
	description	= ""	:: string()}).

-record (parameter, {
	name		= ""	:: string(),
	value		= 0		:: integer(),
	description	= ""	:: string()}).

-record(class, {
	name		= ""	:: string(),
	parents		= []	:: [string()],
	actions		= []	:: [string()],
	stats		= []	:: [string()],
	parameters	= []	:: [string()],
	description	= ""	:: string()}).
