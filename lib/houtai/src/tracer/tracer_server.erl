%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 五月 2018 10:10
%%%-------------------------------------------------------------------
-module(tracer_server).
-author("Administrator").

-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/1, start_as_houtai_child/1, stop_as_houtai_child/1, listen/5, remove_listen/4]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    node
    , trace_dict
}).

-record(tracer_handler, {
    pid
    , type
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
-spec(start_link(Node :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Node) ->
    ServerName = data_mapping:get_tracer_server_name(Node),
    gen_server:start_link({local, ServerName}, ?MODULE, [Node], []).

start_as_houtai_child(Node) ->
    ServerName = data_mapping:get_tracer_server_name(Node),
    AChild = {ServerName, {?MODULE, start_link, [Node]},
        permanent, 2000, worker, [?MODULE]},
    supervisor:start_child(houtai_sup, AChild).


stop_as_houtai_child(Node) ->
    ServerName = data_mapping:get_tracer_server_name(Node),
    supervisor:terminate_child(houtai_sup, ServerName),
    supervisor:delete_child(houtai_sup, ServerName).

listen(Node, Mod, Fun, Type, Handler) ->
    ServerName = data_mapping:get_tracer_server_name(Node),
    gen_server:call(ServerName, {listen, Mod, Fun, Type, Handler}).

remove_listen(Node, Mod, Fun, Handler) ->
    ServerName = data_mapping:get_tracer_server_name(Node),
    gen_server:call(ServerName, {remove_listen, Mod, Fun, Handler}).

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
    Node1 = case is_list(Node) of
                true -> list_to_atom(Node);
                false -> Node
            end,
    {ok, #state{node = Node1, trace_dict = dict:new()}}.

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
handle_call({listen, Mod, Fun, Type, Handler}, {_From, _Tag}, #state{node = Node} = State) when is_atom(Mod) andalso is_atom(Fun) andalso (is_pid(Type) orelse Type =:= all) ->
    case net_adm:ping(Node) of
        pang ->
            {reply, disconnect_node, State};
        pong ->
            prim_listen(Type, Mod, Fun, Handler, State)
    end;
handle_call({remove_listen, Mod, Fun, Handler}, {_From, _Tag}, #state{trace_dict = TraceDict} = State) ->
    case dict:find({Mod, Fun}, TraceDict) of
        error ->
            {reply, ok, State};
        {ok, [#tracer_handler{pid = Handler}]} ->
            dbg:ctpl(Mod, Fun, '_'),
            {reply, ok, State#state{trace_dict = dict:erase({Mod, Fun}, TraceDict)}};
        {ok, List} ->
            {reply, ok, State#state{trace_dict = dict:store({Mod, Fun}, lists:keydelete(Handler, 1, List), TraceDict)}}
    end;
handle_call(Request, _From, State) ->
    houtai_error_logger:error_report("~p handle_call unknow data~np", [?MODULE, Request]),
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
    houtai_error_logger:error_report("~p handle_cast unknow data~n~p", [?MODULE, Request]),
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
handle_info({trace, CallerPid, call, {Mod, Fun, Arg}, _FromMFA}, #state{trace_dict = TraceDict} = State) ->
    case dict:find({Mod, Fun}, TraceDict) of
        error ->
            {noreply, State};
        {ok, []} ->
            dbg:ctpl(Mod, Fun, '_'),
            {noreply, State};
        {ok, List} ->
            NewList =
                lists:foldl(fun(#tracer_handler{pid = Handler, type = Type} = TracerHandler, NewList1) ->
                    case node(Handler) =:= node() of
                        true ->
                            ProcessAliveMod = erlang,
                            ProcessAliveFun = process_info;
                        false ->
                            ProcessAliveMod = rpc,
                            ProcessAliveFun = pinfo
                    end,
                    case ProcessAliveMod:ProcessAliveFun(Handler, status) of
                        undefined ->
                            NewList1;
                        _ ->
                            case Type of
                                all ->
                                    Handler ! {trace, CallerPid, Mod, Fun, Arg};
                                CallerPid ->
                                    Handler ! {trace, CallerPid, Mod, Fun, Arg};
                                _ ->
                                    skip
                            end,
                            [TracerHandler | NewList1]
                    end
                end, [], List),
            case NewList of
                [] ->
                    dbg:ctpl(Mod, Fun, '_'),
                    {noreply, State#state{trace_dict = dict:erase({Mod, Fun}, State#state.trace_dict)}};
                _ ->
                    {noreply, State#state{trace_dict = dict:store({Mod, Fun}, NewList, State#state.trace_dict)}}
            end
    end;
handle_info(Info, State) ->
    houtai_error_logger:error_report("~p handle_info unknow data~n~p", [?MODULE, Info]),
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
prim_listen(Type, Mod, Fun, Handler, State) ->
    case dbg:get_tracer(State#state.node) of
        {ok, _TracePid} ->
            State1 = State,
            skip;
        _ ->
            dbg:ctpl(),
            State1 = State#state{trace_dict = dict:new()},
            F = util:eval_string("fun(Msg, TracerServer) -> TracerServer ! Msg, TracerServer end."),
            {ok, _} = dbg:tracer(State#state.node, process, {F, self()}),
            dbg:p(all, c)
    end,
    #state{trace_dict = TraceDict} = State1,
    {ok, _Ref} = dbg:tpl(Mod, Fun, '_', c),
    NewTracerHandler = #tracer_handler{pid = Handler, type = Type},
    case dict:find({Mod, Fun}, TraceDict) of
        error ->
            {reply, ok, State#state{trace_dict = dict:store({Mod, Fun}, [NewTracerHandler], TraceDict)}};
        {ok, TracerHandlerList} ->
            {reply, ok, State#state{trace_dict = dict:store({Mod, Fun}, lists:keystore(Handler, #tracer_handler.pid, TracerHandlerList, NewTracerHandler), TraceDict)}}
    end.