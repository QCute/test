%% application
-module(test_app).
-compile(export_all).
-behaviour(application).
%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    test_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
