%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 三月 2018 10:57
%%%-------------------------------------------------------------------
-module(houtai_sup).
-author("Administrator").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(LogFile::list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(LogFile) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LogFile]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([LogFile]) ->

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    _AChild = {'AName', {'AModule', start_link, []},
        Restart, Shutdown, Type, ['AModule']},

    ErrLog = {houtai_error_logger, {'houtai_error_logger', start_link, [LogFile]},
        permanent, brutal_kill, worker, ['houtai_error_logger']},
    
    {ok, {SupFlags, [ErrLog]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
