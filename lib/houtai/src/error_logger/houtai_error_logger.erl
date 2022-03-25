%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. 四月 2018 16:24
%%%-------------------------------------------------------------------
-module(houtai_error_logger).
-author("Administrator").

-behaviour(gen_event).

%% API
-export([start_link/1,
    add_handler/0]).

%% gen_event callbacks
-export([init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% public
-export([
    error_msg/1
    , error_msg/2
    , error_report/1
    , error_report/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    file_path       %% 日志文件路径
    , binding_list   %% 绑定列表
}).

%%%===================================================================
%%% public
%%%===================================================================

error_msg(String) when is_list(String) ->
    error_msg(String, []).
error_msg(Format, Args) ->
    case whereis(?MODULE) of
        undefined -> skip;
        _ -> gen_event:notify(?MODULE, {error_msg, self(), Format, Args})
    end.

error_report(String) when is_list(String) ->
    error_report(String, []).
error_report(Format, Args) ->
    case whereis(?MODULE) of
        undefined -> skip;
        _ ->
            gen_event:notify(?MODULE, {error_report, self(), Format, Args})
    end.


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(LogFile :: list()) -> {ok, pid()} | {error, {already_started, pid()}}).
start_link(LogFile) ->
    case gen_event:start_link({local, ?SERVER}) of
        {ok, Pid} ->
            ok = gen_event:add_handler(?MODULE, ?MODULE, [LogFile]),
            {ok, Pid};
        Err -> Err
    end.

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_handler() -> ok | {'EXIT', Reason :: term()} | term()).
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(InitArgs :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, hibernate} |
    {error, Reason :: term()}).
init([LogFile]) ->
    process_flag(trap_exit, true),
    {ok, #state{file_path = LogFile, binding_list = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), State :: #state{}) ->
    {ok, NewState :: #state{}} |
    {ok, NewState :: #state{}, hibernate} |
    {swap_handler, Args1 :: term(), NewState :: #state{},
        Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler).
handle_event(Event, State) ->
    {{Y, Mo, D}, {H, Mi, S}} = erlang:localtime(),
    Head = lists:flatten(io_lib:format("Time ~p-~p-~p ~p:~p:~p~n", [Y, Mo, D, H, Mi, S])),
    {ok, FileDevice} = file:open(State#state.file_path, [append]),
    NewState = case Event of
                   {Tag, _From, Format, Args} when is_atom(Tag) ->
                       case catch io_lib:format(Format, Args) of
                           Msg when is_list(Msg) ->
                               %% error_msg类型的时间才会向前端报告
                               case Tag =:= error_msg of
                                   true ->
                                       NewList = lists:foldl(fun(Pid, List) ->
                                           case erlang:is_process_alive(Pid) of
                                               true ->
                                                   Pid ! {error_msg, Head ++ Msg},
                                                   List;
                                               false -> lists:delete(Pid, List)
                                           end
                                       end, State#state.binding_list, State#state.binding_list);
                                   false ->
                                       NewList = State#state.binding_list
                               end,
                               TagString = lists:flatten(io_lib:format("Tag : ~p~n", [Tag])),
                               io:format(FileDevice, Head ++ TagString ++ Msg ++ "~n", []),
                               State#state{binding_list = NewList};
                           _ ->
                               io:format(FileDevice, Head ++ "bad error~n Format : ~p~n Args : ~p~n", [Format, Args]),
                               State
                       end;
                   _ ->
                       io:format(FileDevice, Head ++ "Unknow event : ~p~n", [Event]),
                       State
               end,
    file:close(FileDevice),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), State :: #state{}) ->
    {ok, Reply :: term(), NewState :: #state{}} |
    {ok, Reply :: term(), NewState :: #state{}, hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
        Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}).
handle_call({add, Pid}, State) when is_pid(Pid) ->
    Reply = ok,
    case lists:member(Pid, State#state.binding_list) of
        false ->
            {ok, Reply, State#state{binding_list = [Pid | State#state.binding_list]}};
        true ->
            {ok, Reply, State}
    end;
handle_call({delete, Pid}, State) when is_pid(Pid) ->
    Reply = ok,
    {ok, Reply, State#state{binding_list = lists:delete(Pid, State#state.binding_list)}};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), State :: #state{}) ->
    {ok, NewState :: #state{}} |
    {ok, NewState :: #state{}, hibernate} |
    {swap_handler, Args1 :: term(), NewState :: #state{},
        Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler).
handle_info(_Info, State) ->
    io:format("handle_info~n~p~n", [_Info]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Args :: (term() | {stop, Reason :: term()} | stop |
remove_handler | {error, {'EXIT', Reason :: term()}} |
{error, term()}), State :: term()) -> term()).
terminate(Reason, _State) ->
    io:format("houtai_error_logger down!~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================