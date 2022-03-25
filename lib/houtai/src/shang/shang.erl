%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. ÆßÔÂ 2018 9:57
%%%-------------------------------------------------------------------
-module(shang).
-author("Administrator").

-include("shang.hrl").

-export([
    start/0
    , stop/0
]).


start() ->
    {ok, RootDir} = file:get_cwd(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/shang_ws", shang_handler, []},
            {"/shang_test_date", cowboy_static, {file, RootDir ++ "/src/shang/web/test_date.html"}},
            {"/shang_pro", cowboy_static, {file, RootDir ++ "/src/shang/web/pro.html"}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(?NodeName, 10, [{port, ?HttpPort}],
        [{env, [{dispatch, Dispatch}]}]),

    try
        MysqlPoolName = data_mapping:get_mysql_pool_name(?NodeName),
        {'ok', _} = mysql_poolboy:add_pool(MysqlPoolName, [], [{host, ?MysqlHost}, {user, ?MysqlUser}, {password, ?MysqlPassword}, {database, ?MysqlDatabase},{keep_alive,true}]),
        {ok, _} = record_server:start_as_houtai_child(?NodeName,?LocalServerRootDir ++ "/include"),
        {ok, _} = lock_server:start_as_houtai_child(?NodeName),
        {ok, _} = tracer_server:start_as_houtai_child(?NodeName),
        {start, ?NodeName}
    catch
        Class:ExceptionPattern ->
            MysqlPoolName1 = data_mapping:get_mysql_pool_name(?NodeName),
            cowboy:stop_listener(?NodeName),
            mysql_poolboy:remove_pool(MysqlPoolName1),
            record_server:stop_as_houtai_child(?NodeName),
            lock_server:stop_as_houtai_child(?NodeName),
            tracer_server:stop_as_houtai_child(?NodeName),
            houtai_error_logger:error_report("error in ~p:start~n~p : ~p", [?MODULE, Class,ExceptionPattern])
    end.

stop() ->
    cowboy:stop_listener(?NodeName),
    MysqlPoolName = data_mapping:get_mysql_pool_name(?NodeName),
    mysql_poolboy:remove_pool(MysqlPoolName),
    record_server:stop_as_houtai_child(?NodeName),
    lock_server:stop_as_houtai_child(?NodeName),
    tracer_server:stop_as_houtai_child(?NodeName).
