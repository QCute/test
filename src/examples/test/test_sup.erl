%% supervisor
-module(test_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([start_child/1, start_link_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name) ->
    start_link(),
    ChildSpecs = {Name, {test_server, start, [Name]}, transient, infinity, worker, [Name]},
    supervisor:start_child(?MODULE, ChildSpecs).

start_link_child(Name) ->
    start_link(),
    ChildSpecs = {Name, {test_server, start_link, [Name]}, transient, infinity, worker, [Name]},
    supervisor:start_child(?MODULE, ChildSpecs).

init([]) ->
    {ok, {{one_for_one, 1000, 1}, []}}.
