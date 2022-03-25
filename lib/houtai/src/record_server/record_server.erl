%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 四月 2018 14:36
%%%-------------------------------------------------------------------
-module(record_server).
-author("Administrator").

-behaviour(gen_server).

%% API
-export([start_link/2, get_records_names/1, get_record_info/2, start_as_houtai_child/2, stop_as_houtai_child/1]).

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
    , record_dict = []
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
-spec(start_link(Node :: atom(), RecordDir :: list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Node, RecordDir) ->
    ServerName = data_mapping:get_record_server_name(Node),
    gen_server:start_link({local, ServerName}, ?MODULE, [Node, RecordDir], []).

start_as_houtai_child(Node, RecordDir) ->
    ServerName = data_mapping:get_record_server_name(Node),
    AChild = {ServerName, {?MODULE, start_link, [Node, RecordDir]},
        permanent, 2000, worker, [?MODULE]},
    supervisor:start_child(houtai_sup, AChild).

stop_as_houtai_child(Node) ->
    ServerName = data_mapping:get_record_server_name(Node),
    supervisor:terminate_child(houtai_sup, ServerName),
    supervisor:delete_child(houtai_sup, ServerName).

get_records_names(Node) ->
    ServerName = data_mapping:get_record_server_name(Node),
    gen_server:call(ServerName, get_records_names).

get_record_info(Node, RecordName) ->
    ServerName = data_mapping:get_record_server_name(Node),
    gen_server:call(ServerName, {get_record_info, RecordName}).

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
init([Node, RecordDir]) ->
    FileList = filelib:wildcard(RecordDir ++ "/*.hrl"),
    Dict = lists:foldl(fun make_records/2, dict:new(), FileList),
    {ok, #state{node = Node, record_dict = Dict}}.

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
handle_call({get_record_info, RecordName}, _From, State) ->
    if
        is_atom(RecordName) -> RecordName1 = atom_to_list(RecordName);
        true -> RecordName1 = RecordName
    end,
    {reply, dict:find(RecordName1, State#state.record_dict), State};
handle_call(get_records_names, _From, State) ->
    {reply, dict:fetch_keys(State#state.record_dict), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
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
handle_info(Info, State) ->
    houtai_error_logger:error_report("~p handle_info unknow data~np", [?MODULE,Info]),
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
terminate(Reason, _State) ->
    houtai_error_logger:error_report("~p terminate Reason~np", [?MODULE,Reason]),
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
%% 硬构造记录信息,记录应该有如下格式
%% -record(name, {
%%     fields = value        %% string
%% }).
%% 返回字典,key=RecordName,value=[{RecordName,Annotate},{fields,Annotate}```````]
make_records(FileName, Dict) ->
    {ok,Forms} = epp_dodger:quick_parse_file(FileName),
    CommentList = erl_comment_scan:file(FileName),
    NewDict = make_record(Forms,CommentList,Dict),
    NewDict.

make_record([],_CommentList,Dict)->
    Dict;
make_record([{attribute,Line,record,{RecordName,FieldTreeList}}|T],CommentList,Dict)->
    {Comment,CommentList1}=record_comment_find(Line,CommentList),
    {FieldList,CommentList2}=make_record_field(FieldTreeList,CommentList1),
    RecordInfoList = [{atom_to_list(RecordName),Comment}|FieldList],
    make_record(T,CommentList2,dict:store(atom_to_list(RecordName),RecordInfoList,Dict));
make_record([_|T],CommentList,Dict)->
    make_record(T,CommentList,Dict).


make_record_field(FieldTreeList,CommentList1)->
    make_record_field(FieldTreeList,CommentList1,[]).
make_record_field([],CommentList,FieldsResultList)->
    {lists:reverse(FieldsResultList),CommentList};
make_record_field([{record_field,Line,{atom,_Line1,FieldName},_DefaultValue}|T],CommentList,FieldsResultList)->
    {Comment,CommentList1} = field_comment_find(Line,CommentList),
    make_record_field(T,CommentList1,[{atom_to_list(FieldName),Comment}|FieldsResultList]);
make_record_field([{record_field,Line,{atom,_Line1,FieldName}}|T],CommentList,FieldsResultList)->
    {Comment,CommentList1} = field_comment_find(Line,CommentList),
    make_record_field(T,CommentList1,[{atom_to_list(FieldName),Comment}|FieldsResultList]).


record_comment_find(_RecordLine,[])->
    {"",[]};
record_comment_find(RecordLine,[{CommentLine1,_Column,_Indentation,Comment}|T] = CommentList) ->
    if
        CommentLine1 =:= RecordLine - 1 orelse CommentLine1 =:= RecordLine->
            {comment_tidy(hd(Comment)),T};
        CommentLine1 < RecordLine->
            record_comment_find(RecordLine,T);
        true ->
            {"",CommentList}
    end.

field_comment_find(_FieldLine,[])->
    {"",[]};
field_comment_find(FieldLine,[{CommentLine1,_Column,_Indentation,Comment}|T] = CommentList)->
    if
        CommentLine1 =:= FieldLine ->
            {comment_tidy(hd(Comment)),T};
        CommentLine1 < FieldLine ->
            field_comment_find(FieldLine,T);
        true ->
            {"",CommentList}
    end.


comment_tidy([$%|T])->
    comment_tidy(T);
comment_tidy([$ |T])->
    comment_tidy(T);
comment_tidy(Comment)->
    Comment.
