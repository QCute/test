%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. ÆßÔÂ 2018 11:00
%%%-------------------------------------------------------------------
-module(lock_server).
-author("Administrator").

-behaviour(gen_server).

%% API
-export([start_link/1, lock/2, unlock/2, start_as_houtai_child/1, stop_as_houtai_child/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    node = none
    ,run_list = []
}).

-record(run,{
    tag = none
    ,state = waiting
    ,who = none
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Node::atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Node) ->
    ServerName = data_mapping:get_lock_server_name(Node),
    gen_server:start_link({local, ServerName}, ?MODULE, [Node], []).

start_as_houtai_child(Node)->
    ServerName = data_mapping:get_lock_server_name(Node),
    AChild = {ServerName, {?MODULE, start_link, [Node]},
        permanent, 2000, worker, [?MODULE]},
    supervisor:start_child(houtai_sup,AChild).

stop_as_houtai_child(Node)->
    ServerName = data_mapping:get_lock_server_name(Node),
    supervisor:terminate_child(houtai_sup,ServerName),
    supervisor:delete_child(houtai_sup,ServerName).

-spec lock(Node::atom(),Tag :: term()) -> ok|fail.
lock(Node,Tag) ->
    ServerName = data_mapping:get_lock_server_name(Node),
    gen_server:call(ServerName, {lock_run, Tag}).

-spec unlock(Node::atom(),Tag :: term()) -> ok|fail.
unlock(Node,Tag) ->
    ServerName = data_mapping:get_lock_server_name(Node),
    gen_server:call(ServerName, {unlock_run, Tag}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Node]) ->
    {ok, #state{node = Node}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({lock_run,Tag}, {Pid,_Ref}, State) ->
    case lists:keyfind(Tag,#run.tag,State#state.run_list) of
        #run{state = running} ->
            {reply, fail, State};
        _ ->
            NewLockList = lists:keystore(Tag, #run.tag, State#state.run_list, #run{tag = Tag,state = running,who = Pid}),
            {reply, ok, State#state{run_list = NewLockList}}
    end;
handle_call({unlock_run,Tag}, {Pid,_Ref}, State) ->
    case lists:keyfind(Tag,#run.tag,State#state.run_list) of
        #run{tag = Tag,state = running,who = Pid} ->
            NewLockList = lists:keystore(Tag, #run.tag, State#state.run_list, #run{tag = Tag,state = waiting,who = none}),
            {reply, ok, State#state{run_list = NewLockList}};
        _ ->
            {reply, ok, State}
    end;
handle_call(Request, _From, State) ->
    houtai_error_logger:error_report("~p handle_call unknow data~np", [?MODULE,Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
    houtai_error_logger:error_report("~p handle_cast unknow data~np", [?MODULE,Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    houtai_error_logger:error_report("~p handle_info unknow data~np", [?MODULE,_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    houtai_error_logger:error_report("~p terminate _Reason~np", [?MODULE,_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================