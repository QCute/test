%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 三月 2018 10:56
%%%-------------------------------------------------------------------
-module(shang_handler).
-author("Administrator").

-behaviour(houtai_handler).

-include("shang.hrl").
-include("common.hrl").

%% API
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([
    spawn_link_handler/3
    , record_server_field_try_fetch_name/3
]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    %% 添加到错误日志
    ok = gen_event:call(houtai_error_logger, houtai_error_logger, {add, self()}),
    {ok, Req, #shang_state{worker = none}}.


websocket_handle({text, _Msg}, Req, #shang_state{worker = Worker} = State) when is_pid(Worker) ->
    {reply, {text, <<"running!">>}, Req, State};
websocket_handle({text, Msg}, Req, State) ->
    SpawnPid = spawn_link(node(), ?MODULE, spawn_link_handler, [self(), Msg, State]),
    {reply, {text, <<"please waiting!">>}, Req, State#shang_state{worker = SpawnPid}};
websocket_handle({pong,<<>>}, Req, State) ->
    {ok, Req, State};
websocket_handle(Data, Req, State) ->
    houtai_error_logger:error_report("~p:websocket_handle unknow Data~n~p", [?MODULE,Data]),
    {ok, Req, State}.

%% 接收信息
websocket_info({worker_return, RetuenMsg, NewState}, Req, _State) ->
    {reply, {text, RetuenMsg}, Req, NewState#shang_state{worker = none}};
websocket_info({error_msg, RetuenMsg}, Req, State) ->
    {reply, {text, RetuenMsg}, Req, State};
websocket_info({trace, CallerPid, Mod, Fun, Args}, Req, State) ->
    try
        Msg = case Fun of
                  handle ->
                      [Cmd, PlayerState, Bin] = Args,
                      {PlayerId, PlayerName} = {element(2, PlayerState), element(3, PlayerState)},
                      io_lib:format("玩家 ~p(~s) : 收到~n协议号 : ~p~n~p", [PlayerId, PlayerName, Cmd, Bin]);
                  write ->
                      [Cmd, Bin] = Args,
                      {PlayerId, PlayerName} = try_get_pid_id_and_name(CallerPid),
                      io_lib:format("玩家 ~p(~s) : 发送~n协议号 : ~p~n~p", [PlayerId, PlayerName, Cmd, Bin]);
                  _ ->
                      {PlayerId, PlayerName} = try_get_pid_id_and_name(CallerPid),
                      lists:flatten(io_lib:format(<<"玩家 ~p(~s) : 未知操作~n ~p:~p(~p)">>, [PlayerId, PlayerName, Mod, Fun, Args]))
              end,
        {reply, {text, Msg}, Req, State}
    catch
        _:_ ->
            houtai_error_logger:error_report("error in websocket_handle:spawn_link_handler with TracerMsg~n~p", [{trace, CallerPid, Mod, Fun, Args}]),
            {ok, Req, State}
    end;
websocket_info({'EXIT', SpawnPid, Reason}, Req, #shang_state{worker = SpawnPid} = State) ->
    houtai_error_logger:error_report("error in websocket_handle:spawn_link_handler with Reason~n~p", [Reason]),
    {shutdown, Req, State};
websocket_info(Info, Req, State) ->
    houtai_error_logger:error_report("~p:websocket_info unknow data~n~p", [?MODULE, Info]),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
    ok = gen_event:call(houtai_error_logger, houtai_error_logger, {delete, self()}).


spawn_link_handler(From, Msg, State) ->
    case lock_server:lock(?NodeName, ?NodeName) of
        ok ->
            case catch do_spawn_link_handler(From, Msg) of
                RetuenMsg when is_list(RetuenMsg) orelse is_binary(RetuenMsg) ->
                    skip;
                Err ->
                    houtai_error_logger:error_report("error in do_spawn_link_handler~n~p", [Err]),
                    RetuenMsg = <<"未知错误,找管理员">>
            end,
            ok = lock_server:unlock(?NodeName, ?NodeName);
        fail ->
            RetuenMsg = <<"其他用户正在使用!">>
    end,
    From ! {worker_return, RetuenMsg, State}.
do_spawn_link_handler(From, Msg) ->
    [Operation | Content] = binary:split(Msg, <<";;;">>),
    case Operation of
        <<"stop">> ->
            stop_node(),
            <<"关闭成功!">>;
        <<"restart">> ->
            stop_node(),
            case start_node() of
                ok -> <<"重启成功!">>;
                time_out -> <<"启动超时,请联系管理员!">>
            end;
        <<"start">> ->
            case util:get_data(hd(Content), [int, int, int]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Day, Hour, Min] when Day < 1 orelse Hour < 0 orelse Hour >= 24 orelse Min < 0 orelse Min >= 60 ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Day, Hour, Min] ->
                    NewTime = calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds({?OpenDate, {0, 0, 0}}) + (Day - 1) * 86400 + Hour * 3600 + Min * 60),
                    stop_node(),
                    MysqlPoolName = data_mapping:get_mysql_pool_name(?NodeName),
                    TruncateSql = io_lib:format(<<"SELECT  concat('truncate table ',table_name,';') from information_schema.`TABLES` WHERE table_schema = '~s' and table_name like 'st_%'">>,[?MysqlDatabase]),
                    {ok, _, Sqls} = mysql_poolboy:query(MysqlPoolName, TruncateSql),
                    lists:foreach(fun([Sql]) -> mysql_poolboy:query(MysqlPoolName, Sql) end, Sqls),
                    change_time(NewTime),
                    case start_node() of
                        ok -> io_lib:format(<<"开服后 ~p天, ~p小时, ~p分钟开启成功!">>, [Day, Hour, Min]);
                        time_out -> <<"启动超时,请联系管理员!">>
                    end
            end;
        <<"truncate">> ->
            stop_node(),
            MysqlPoolName = data_mapping:get_mysql_pool_name(?NodeName),
            TruncateSql = io_lib:format(<<"SELECT  concat('truncate table ',table_name,';') from information_schema.`TABLES` WHERE table_schema = '~s' and table_name not like 'base_%'">>,[?MysqlDatabase]),
            {ok, _, Sqls} = mysql_poolboy:query(MysqlPoolName, TruncateSql),
            lists:foreach(fun([Sql]) -> mysql_poolboy:query(MysqlPoolName, Sql) end, Sqls),
            <<"清档完成!现在可以开启服务器了!">>;
        <<"update">> ->
            stop_node(),
            NetTime = util:get_net_time(),
            change_time(NetTime),
            copy_svn(),
            make_all(),
            <<"更新完成!现在可以开启服务器了!">>;
        <<"updateTime">> ->
            NetTime = util:get_net_time(),
            change_time(NetTime),
            {{Y, Mo, D}, {H, Mi, S}} = calendar:local_time(),
            io_lib:format(<<"成功更新时间!~n现在是 ~p-~p-~p ~p:~p:~p">>, [Y, Mo, D, H, Mi, S]);
        <<"shutdown">> ->
            stop_node(),
            Min = 1,
            shutdown_computer(Min),
            io_lib:format(<<"~p分钟后关闭计算机">>, [Min]);
        <<"player_status">> ->
            case util:get_data(hd(Content), [int]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id] when Id < 0 ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id] ->
                    case get_player_server_name(Id) of
                        PlayerServerName when is_atom(PlayerServerName) ->
                            case rpc_apply(sys, get_state, [PlayerServerName]) of
                                {badrpc, {'EXIT', {noproc, _}}} ->
                                    io_lib:format(<<"玩家~p,未登录!">>, [Id]);
                                {badrpc, Err} ->
                                    houtai_error_logger:error_report("error in do_spawn_link_handler player_status~nUnknow Err ~p", [Err]),
                                    <<"未知错误,找管理员">>;
                                PlayerStatus ->
                                    record_server_util:try_print_record({?AtomNodeName,?MODULE}, PlayerStatus)
                            end;
                        _Err ->
                            <<"游戏服务器未启动">>
                    end
            end;
        <<"player_dict_keys">> ->
            case util:get_data(hd(Content), [int]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id] when Id < 0 ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id] ->
                    case rpc_player_apply_mfa(Id, erlang, get, []) of
                        {error, ErrorMsg} ->
                            ErrorMsg;
                        {ok, Result} ->
                            io_lib:format("~p", [[K || {K, _} <- Result]])
                    end
            end;
        <<"player_dict_key">> ->
            case util:get_data(hd(Content), [int, atom]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id, _] when Id < 0 ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id, Key] ->
                    case rpc_player_apply_mfa(Id, erlang, get, [Key]) of
                        {error, ErrorMsg} ->
                            ErrorMsg;
                        {ok, undefined} ->
                            <<"不存在的key!">>;
                        {ok, Result} ->
                            record_server_util:try_print_record({?AtomNodeName,?MODULE}, Result)
                    end
            end;
        <<"test_cmd">> ->
            case util:get_data(hd(Content), [int, int, term]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id, Pt, Arg] ->
                    case rpc_apply(util, test_cmd, [Id, Pt, Arg]) of
                        ok ->
                            io_lib:format(<<"测试协议,玩家id ~p,协议号 ~p,参数 ~p">>, [Id, Pt, Arg]);
                        false ->
                            io_lib:format(<<"玩家~p,未登录!">>, [Id]);
                        Err ->
                            houtai_error_logger:error_report("error in do_spawn_link_handler test_cmd~nUnknow Err ~p", [Err]),
                            <<"未知错误,找管理员">>
                    end
            end;
        <<"record_names">> ->
            io_lib:format("~p", [record_server:get_records_names(?AtomNodeName)]);
        <<"record_info">> ->
            case util:get_data(hd(Content), [atom]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Name] ->
                    record_server_util:print_record(?AtomNodeName, Name)
            end;
        <<"record_data">> ->
            record_server_util:try_print_string_record({?AtomNodeName,?MODULE}, hd(Content));
        <<"now">> ->
            {Day, {Hour, Min, Sec}} = calendar:time_difference({?OpenDate, {0, 0, 0}}, calendar:local_time()),
            io_lib:format(<<"现在的时间是,开服第~p天 ~p:~p:~p">>, [Day+1, Hour, Min, Sec]);
        <<"string">> ->
            case util:get_data(hd(Content), [term]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [String] ->
                    io_lib:format("~s", [String])
            end;
        <<"unixtime">> ->
            case util:get_data(hd(Content), [int]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [UnixTime] ->
                    {{Year, Month, Day}, {Hour, Min, Sec}} = util:unixtime_to_date(UnixTime),
                    io_lib:format(<<"时间戳转换结果~n~p -> ~p-~p-~p ~p:~p:~p">>, [UnixTime, Year, Month, Day, Hour, Min, Sec])
            end;
        <<"trace">> ->
            case util:get_data(hd(Content), [int, int]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Pt, Id] when Pt < 100 orelse Pt > 999 orelse Id < 0 ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Pt, Id] ->
                    case get_pp_name(Pt) of
                        undefined ->
                            io_lib:format(<<"参数错误~n~s!">>, [Content]);
                        PpNme ->
                            Type = case Id of
                                       0 ->
                                           all;
                                       _ ->
                                           case get_player_server_name(Id) of
                                               PlayerServerName when is_atom(PlayerServerName) ->
                                                   case rpc_apply(erlang, whereis, [PlayerServerName]) of
                                                       undefined ->
                                                           io_lib:format(<<"玩家~p,未登录!">>, [Id]);
                                                       PlayerPid ->
                                                           PlayerPid
                                                   end;
                                               _Err ->
                                                   <<"游戏服务器未启动">>
                                           end
                                   end,
                            case is_pid(Type) orelse Type=:=all of
                                false -> Type;
                                true ->
                                    case catch tracer_server:listen(?NodeName, PpNme, handle, Type, From) of
                                        disconnect_node -> <<"无法连接游戏节点!">>;
                                        ok ->
                                            PtName = get_pt_name(Pt),
                                            ok = tracer_server:listen(?NodeName, PtName, write, Type, From),
                                            io_lib:format(<<"监听协议 ~p--~p!">>, [PtName, PpNme]);
                                        Err ->
                                            houtai_error_logger:error_report("error in do_spawn_link_handler trace~nUnknow Err ~p", [Err]),
                                            <<"监听协议未知错误,找管理员">>
                                    end
                            end
                    end
            end;
        <<"untrace">> ->
            case util:get_data(hd(Content), [int]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Pt] when Pt < 100 orelse Pt > 999 ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Pt] ->
                    case get_pp_name(Pt) of
                        undefined ->
                            io_lib:format(<<"参数错误~n~s!">>, [Content]);
                        PpNme ->
                            case catch tracer_server:remove_listen(?NodeName, PpNme, handle, From) of
                                ok ->
                                    PtName = get_pt_name(Pt),
                                    tracer_server:remove_listen(?NodeName, PtName, write, From),
                                    io_lib:format(<<"取消监听协议 ~p--~p!">>, [PtName, PpNme]);
                                Err ->
                                    houtai_error_logger:error_report("error in do_spawn_link_handler untrace~nUnknow Err ~p", [Err]),
                                    <<"监听协议未知错误,找管理员">>
                            end
                    end
            end;
        <<"houtai_execute">> ->
            case util:get_data(hd(Content), [term]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Return] ->
                    io_lib:format("~p", [Return])
            end;
        <<"player_execute">> ->
            case util:get_data(hd(Content), [int, term, term]) of
                bad_argument ->
                    io_lib:format(<<"参数错误~n~s!">>, [Content]);
                [Id, Fun, Arg] ->
                    case net_adm:ping(?AtomNodeName) of
                        pang ->
                            <<"无法连接游戏节点!">>;
                        pong ->
                            case rpc_player_apply_fun(Id, Fun, Arg) of
                                {error, ErrorMsg} ->
                                    ErrorMsg;
                                {ok, Result} ->
                                    io_lib:format("~p", [Result])
                            end
                    end
            end;
        _ ->
            houtai_error_logger:error_report("error in do_spawn_link_handler ~nUnknow Msg ~p", [Msg]),
            io_lib:format("未知指令 ~p", [Msg])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Helper %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start_node() ->
    case os:type() of
        {unix, _} ->
            os:cmd(lists:flatten(io_lib:format("cd ~s/config;erl +P 1024000 +K true -smp disable -name ~s -setcookie ~s -boot start_sasl -config gs -pa ../ebin -s gs start -extra ~s ~p 11 &", [?LocalServerRootDir, ?NodeName, ?ErlCookie, ?NodeIp, ?NodePort])));
        {win32, _} ->
            util:no_return_cmd("start /min erl +P 1024000 +K true -smp disable -name ~s -setcookie ~s -boot start_sasl -config gs -pa ../ebin -s gs start -extra ~s ~p 11", [?NodeName, ?ErlCookie, ?NodeIp, ?NodePort], [{cd, ?LocalServerRootDir ++ "/config"}])
    end,
    start_loop(0).

start_loop(Time) ->
    case rpc_apply(erlang, whereis, [?CheckStartServerName]) of
        Pid when is_pid(Pid) ->
            ok;
        _Err ->
            timer:sleep(500),
            case Time > 60*1000 of
                true ->
                    time_out;
                false->
                    start_loop(Time+500)
            end
    end.

stop_node() ->
    case os:type() of
        {unix, _} ->
            os:cmd(lists:flatten(io_lib:format("erl -noshell -name ~s -setcookie ~s -eval \"rpc:cast('~s', gs, stop, [])\" -eval \"timer:sleep(500)\" -eval \"init:stop()\"", ["stop_" ++ ?NodeName, ?ErlCookie, ?NodeName])));
        {win32, _} ->
            util:synchronization_cmd("erl -noshell -name ~s -setcookie ~s -eval \"rpc:cast('~s', gs, stop, [])\" -eval \"timer:sleep(500)\" -eval \"init:stop()\"", ["stop_" ++ ?NodeName, ?ErlCookie, ?NodeName], [hide])
    end,

    stop_loop().
stop_loop() ->
    case net_adm:ping(?AtomNodeName) of
        pang ->
            ok;
        pong ->
            timer:sleep(500),
            stop_loop()
    end.

change_time({Date, Time}) ->
    case os:type() of
        {unix, _} ->
            os:cmd(lists:flatten(io_lib:format("date -s \"~p-~p-~p ~p:~p:~p\"", lists:flatten(tuple_to_list(Date), tuple_to_list(Time)))));
        {win32, _} ->
            util:synchronization_cmd("date ~p-~p-~p & time ~p:~p:~p", lists:flatten(tuple_to_list(Date), tuple_to_list(Time)), [hide])
    end.


copy_svn() ->
    case os:type() of
        {unix, _} ->
            os:cmd(lists:flatten(io_lib:format("svn cleanup \"~s\"", [ ?LocalServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("svn update \"~s\"", [?SvnServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("rm -rf \"~s/ebin\"", [ ?LocalServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("rm -rf \"~s/include\"", [ ?LocalServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("rm -rf \"~s/src\"", [ ?LocalServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("cp -R \"~s/ebin\" \"~s\"", [?SvnServerRootDir, ?LocalServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("cp -R \"~s/include\" \"~s\"", [?SvnServerRootDir, ?LocalServerRootDir]))),
            os:cmd(lists:flatten(io_lib:format("cp -R \"~s/src\" \"~s\"", [?SvnServerRootDir, ?LocalServerRootDir])));
        {win32, _} ->
            util:synchronization_cmd("svn update \"~s/src/data\"", [?SvnServerRootDir], [hide]),
            util:synchronization_cmd("rmdir /s/q \"~s/ebin\"", [?LocalServerRootDir], [hide]),
            util:synchronization_cmd("md \"~s/ebin\"", [?LocalServerRootDir], [hide]),
            util:synchronization_cmd("rmdir /s/q \"~s/src/data\"", [?LocalServerRootDir], [hide]),
            util:synchronization_cmd("md \"~s/src/data\"", [?LocalServerRootDir], [hide]),
            util:synchronization_cmd("xcopy /Y /I /e /q \"~s/ebin\" \"~s/ebin\"", [?SvnServerRootDir, ?LocalServerRootDir], [hide]),
            util:synchronization_cmd("xcopy /Y /I /e /q \"~s/src/data\" \"~s/src/data\"", [?SvnServerRootDir, ?LocalServerRootDir], [hide])
    end.

make_all() ->
    case os:type() of
        {unix, _} ->
            os:cmd(lists:flatten(io_lib:format("cd \"~s\" && erl -noshell -noinput -eval \"make:all()\" -eval \"init:stop()\"",[?LocalServerRootDir])));
        {win32, _} ->
            util:synchronization_cmd("erl -noshell -eval \"make:all()\" -eval \"init:stop()\"", [], [{cd, ?LocalServerRootDir}, hide])
    end.

shutdown_computer(Min) ->
    case os:type() of
        {unix, _} ->
            os:cmd(lists:flatten(io_lib:format("shutdown -h -t ~p", [Min])));
        {win32, _} ->
            util:synchronization_cmd("shutdown -s -t ~p", [Min*60], [hide])
    end.


rpc_player_apply_fun(Id, F, A) when is_integer(Id) andalso is_function(F) andalso is_list(A) ->
    case get_player_server_name(Id) of
        {badrpc, _Err} ->
            {error,<<"游戏服务器未启动">>};
        PlayerServerName when is_atom(PlayerServerName) ->
            case rpc_gen_server_call([PlayerServerName, {'apply_cast', erlang, apply, [F, A]}]) of
                {badrpc, {'EXIT', {noproc, _}}} ->
                    {error, io_lib:format(<<"玩家~p,未登录!">>, [Id])};
                {badrpc, Err} ->
                    houtai_error_logger:error_report("error in rpc_player_apply_fun~nUnknow Err ~p", [Err]),
                    {error, <<"未知错误,找管理员">>};
                Result ->
                    {ok, Result}
            end
    end.

rpc_player_apply_mfa(Id, M, F, A) when is_integer(Id) andalso is_atom(M) andalso is_atom(F) andalso is_list(A) ->
    case get_player_server_name(Id) of
        {badrpc, _Err} ->
            {error,<<"游戏服务器未启动">>};
        PlayerServerName when is_atom(PlayerServerName) ->
            case rpc_gen_server_call([PlayerServerName, {'apply_cast', M, F, A}]) of
                {badrpc, {'EXIT', {noproc, _}}} ->
                    {error, io_lib:format(<<"玩家~p,未登录!">>, [Id])};
                {badrpc, Err} ->
                    houtai_error_logger:error_report("error in rpc_player_apply_mfa~nUnknow Err ~p", [Err]),
                    {error, <<"未知错误,找管理员">>};
                Result ->
                    {ok, Result}
            end
    end.

rpc_apply(M, F, A) ->
    rpc:call(?AtomNodeName, M, F, A).

get_player_server_name(Id) ->
    rpc:call(?AtomNodeName, misc, player_process_name, [Id]).

rpc_gen_server_call(Args) ->
    rpc:call(?AtomNodeName, gen_server, call, Args).

get_pp_name(Pt) ->
    case Pt of
        100 -> pp_login;
        110 -> pp_chat;
        111 -> pp_mail;
        120 -> pp_goods;
        121 -> pp_fashion;
        130 -> pp_player;
        140 -> pp_fuzhai;
        150 -> pp_menke;
        160 -> pp_guanka;
        170 -> pp_beauty;
        171 -> pp_heir;
        172 -> pp_yuehui;
        180 -> pp_feast;
        190 -> pp_rank;
        200 -> pp_play;
        201 -> pp_fuben;
        202 -> pp_yamen;
        203 -> pp_hanlin;
        210 -> pp_activity;
        211 -> pp_activity_recharge;
        212 -> pp_penalty;
        213 -> pp_paimai;
        220 -> pp_weal;
        230 -> pp_guild;
        240 -> pp_task;
        _ -> undefined
    end.

get_pt_name(Pt) ->
    list_to_atom("pt_" ++ integer_to_list(Pt)).

try_get_pid_id_and_name(Pid) ->
    case catch sys:get_state(Pid) of
        Tuple when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 andalso element(1, Tuple) =:= player_status ->
            {element(2, Tuple), element(3, Tuple)};
        _Err ->
            {0, <<"未知">>}
    end.


record_server_field_try_fetch_name(_RecordName, FiledName,FieldData) when is_integer(FieldData) ->
    MysqlPoolName = data_mapping:get_mysql_pool_name(?AtomNodeName),
    case FiledName of
        "menke_id" ->
            BaseData = rpc:call(?AtomNodeName, data_menke, get, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "beauty_id" ->
            BaseData = rpc:call(?AtomNodeName, data_beauty, get, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "goods_id" ->
            BaseData = rpc:call(?AtomNodeName, data_goods, get, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "chengjiu_type" ->
            BaseData = rpc:call(?AtomNodeName, data_chengjiu, get_chengjiu, [FieldData, 1]),
            record_server_field_try_fetch_name_helper(BaseData, 4);
        "fashion_id" ->
            case catch mysql_poolboy:query(MysqlPoolName, io_lib:format("select fashion_name from base_fashion where fashion_id = ~p", [FieldData])) of
                {ok, [<<"fashion_name">>], [[FashionName]]} ->
                    record_server_field_try_fetch_name_helper(FashionName);
                _ -> []
            end;
        "feast_id" ->
            BaseData = rpc:call(?AtomNodeName, data_feast, get, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "job" ->
            BaseData = rpc:call(?AtomNodeName, data_job, get, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "prisoner_id" ->
            BaseData = rpc:call(?AtomNodeName, data_prisoner, get, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "car_id" ->
            BaseData = rpc:call(?AtomNodeName, data_yuehui, get_car, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "place_id" ->
            BaseData = rpc:call(?AtomNodeName, data_yuehui, get_place, [FieldData]),
            record_server_field_try_fetch_name_helper(BaseData, 3);
        "role_id" ->
            case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select role_name from player where role_id = ~p", [FieldData])) of
                {ok, [<<"role_name">>], [[RoleName]]} ->
                    record_server_field_try_fetch_name_helper(RoleName);
                _ -> []
            end;
        "guild_id" ->
            case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select guild_name from guild where guild_id = ~p", [FieldData])) of
                {ok, [<<"guild_name">>], [[GuildName]]} ->
                    record_server_field_try_fetch_name_helper(GuildName);
                _ -> []
            end;
        "relation_level" ->
            case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select relation_name from base_beauty_relation where relation_level = ~p", [FieldData])) of
                {ok, [<<"relation_name">>], [[FileName]]} ->
                    record_server_field_try_fetch_name_helper(FileName);
                _ -> []
            end;
        "activity_id" ->
            case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select activity_name from base_activity where activity_id = ~p", [FieldData])) of
                {ok, [<<"activity_name">>], [[FileName]]} ->
                    record_server_field_try_fetch_name_helper(FileName);
                _ ->
                    case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select type_name from base_activity_limit where activity_id = ~p", [FieldData])) of
                        {ok, [<<"type_name">>], [[FileName]]} ->
                            record_server_field_try_fetch_name_helper(FileName);
                        _ ->
                            case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select activity_name from base_activity_rank where activity_id = ~p", [FieldData])) of
                                {ok, [<<"activity_name">>], [[FileName]]} ->
                                    record_server_field_try_fetch_name_helper(FileName);
                                _ -> []
                            end
                    end
            end;
        "office" ->
            case catch mysql_poolboy:query(MysqlPoolName,  io_lib:format("select office_name from base_menke_office where office = ~p", [FieldData])) of
                {ok, [<<"office_name">>], [[FileName]]} ->
                    record_server_field_try_fetch_name_helper(FileName);
                _ ->
                    []
            end;
        _ -> []
    end;
record_server_field_try_fetch_name(_,_, _) -> [].
record_server_field_try_fetch_name_helper(BaseData, Pos) ->
    case is_tuple(BaseData) of
        true ->
            record_server_field_try_fetch_name_helper(element(Pos, BaseData));
        false -> []
    end.
record_server_field_try_fetch_name_helper(Binary) ->
    case is_binary(Binary) of
        true -> "(" ++ binary_to_list(Binary) ++ ")";
        false -> []
    end.