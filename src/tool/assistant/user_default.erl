-module(user_default).
-compile(nowarn_export_all).
-compile(export_all).
%% escript entry
-export([main/1]).
%% supervisor and gen_server callbacks
-include_lib("compiler/src/beam_opcodes.hrl").
-include_lib("compiler/src/beam_disasm.hrl").
%%
-include_lib("inets/include/httpd.hrl").
-include_lib("ssl/src/ssl_api.hrl").
-include_lib("stdlib/include/qlc.hrl").


%%%===================================================================
%%% API
%%%===================================================================

main(_) ->
    
    ok.


tpp() ->
    
    catch ets:delete(test),
    catch ets:new(test, [named_table, set]),
    List = [{X, X} || X <- lists:seq(1, 1000)],
    ets:insert(test, List),
    eflame2:write_trace(global_calls_plus_new_procs, "stacks.out.0", self(), protocol, write_ets, [fun([{X, Y}]) -> <<X:16, Y:16>> end, test]),
    eflame2:format_trace("stacks.out.0", "stacks.out"),
    file:delete("stacks.out.0"),
    os:cmd("cat stacks.out | ./lib/eflame/flamegraph.riak-color.pl > flame.svg"),
    file:delete("stacks.out").
    %eflame:apply(protocol, write_list, [fun({X, Y}) -> <<X:16, Y:16>> end, List]).


%% array [value, ...]
%% object { string: value, ... }
%%

root(Env) ->
    catch ets:new(t, [named_table, ordered_set]),
    List = [{X, X} || X <- lists:seq(1, 10000)],
    ets:insert(t, List),
    eflame:apply(normal, "stacks.out", ?MODULE, t_f, [List]),
    Env.


t_s() ->
    %% D:\work\szfy\local\priv\data\create
    {ok, Data} = file:read_file("/mnt/d/work/szfy/local/priv/data/create/data_beauty.json"),
    Start = os:timestamp(),
    json:decode(Data),
    Middle = os:timestamp(),
    jsone:decode(Data),
    End = os:timestamp(),
    io:format("~p ~p~n", [timer:now_diff(Middle, Start), timer:now_diff(End, Middle)]).

t_mv() ->
    %% D:\work\szfy\local\priv\data\create
    {ok, Data} = file:read_file("/mnt/d/work/szfy/local/priv/data/create/data_beauty.json"),
    Start = os:timestamp(),
    json:decode(Data),
    Middle = os:timestamp(),
    jsone:decode(Data),
    End = os:timestamp(),
    io:format("~p ~p~n", [timer:now_diff(Middle, Start), timer:now_diff(End, Middle)]).


t_mvs() ->
    %% D:\work\szfy\local\priv\data\create
    {ok, M} = mysql_connector:start_link([{host, "192.168.26.40"}, {port, 3306}, {user, "root"}, {password, "root"}, {database, "szfy_kaifa"}, {encoding, "utf8mb4"}]),
    {ok, V} = mysql_connector_v:start_link([{host, "192.168.26.40"}, {port, 3306}, {user, "root"}, {password, "root"}, {database, "szfy_kaifa"}, {encoding, "utf8mb4"}]),

    io:format("start m~n"),
    spawn(fun() ->
        {MT, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> mysql_connector:query(<<"select * from szfy_kaifa.", T/binary>>, M) end, mysql_connector:query("SHOW TABLES", M))
        end),
        io:format("M: ~p:~p~n", [MT, erlang:process_info(M, memory)])
    end),
    io:format("start v~n"),
    spawn(fun() ->
        {VT, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> mysql_connector_v:query(<<"select * from szfy_kaifa.", T/binary>>, V) end, mysql_connector_v:query("SHOW TABLES", V))
        end),
        io:format("V: ~p:~p~n", [VT, erlang:process_info(V, memory)])
    end),

    ok.

test_mysql_connector() ->
    %% D:\work\szfy\local\priv\data\create

    %% mysql
    spawn(fun() ->
        io:format("start mysql~n"),
        {ok, _} = mysql:start_link(1, "192.168.26.40", 3306, "root", "root", "szfy_kaifa", fun(_, _, _, _) -> ok end, utf8mb4),
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> mysql:fetch(1, <<"select * from szfy_kaifa.", T/binary>>) end, element(3, element(2, mysql:fetch(1, "SHOW TABLES"))))
        end),
        io:format("mysql: ~p~n", [T])
    end),
    %% 982400
    %% 838034
    %% 836162

    %% mysql-otp
    spawn(fun() ->
        io:format("start mysql-otp~n"),
        {ok, P} = mysql:start_link([{host, "192.168.26.40"}, {port, 3306}, {user, "root"}, {password, "root"}, {database, "szfy_kaifa"}]),
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> mysql:query(P, <<"select * from szfy_kaifa.", T/binary>>) end, element(3, mysql:query(P, "SHOW TABLES")))
        end),
        io:format("mysql-otp: ~p~n", [T])
    end),
    %% 1520338
    %% 1011661
    %%  987828

    %% emysql
    spawn(fun() ->
        io:format("start mysql-otp~n"),
        crypto:start(),
        application:start(emysql),
        emysql:add_pool(test, [{size,1}, {user,"root"}, {password,"root"}, {database,"szfy_kaifa"}, {encoding, utf8mb4}]),
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> emysql:execute(test, <<"select * from szfy_kaifa.", T/binary>>) end, element(4, emysql:execute(test, "SHOW TABLES")))
        end),
        io:format("emysql: ~p~n", [T])
    end),
    %% 640698
    %% 671094
    %% 653903

    %% p1-mysql
    spawn(fun() ->
        io:format("start p1-mysql~n"),
        {ok, _} = p1_mysql:start_link(1, "192.168.26.40", 3306, "root", "root", "szfy_kaifa", fun(_, _, _) -> ok end),
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> p1_mysql:fetch(1, ["select * from szfy_kaifa.", T]) end, element(3, element(2, p1_mysql:fetch(1, "SHOW TABLES"))))
        end),
        io:format("p1_mysql: ~p~n", [T])
    end),
    %% 1051280
    %% 1091125
    %% 1146160

    %% mysql-connector
    spawn(fun() ->
        io:format("start mysql-connector~n"),
        {ok, M} = mysql_connector:start_link([{host, "192.168.26.40"}, {port, 3306}, {user, "root"}, {password, "root"}, {database, "szfy_kaifa"}, {encoding, "utf8mb4"}]),
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> mysql_connector:query(<<"select * from szfy_kaifa.", T/binary>>, M) end, mysql_connector:query("SHOW TABLES", M))
        end),
        io:format("mysql-connector: ~p~n", [T])
    end),
    %% 636052
    %% 630514
    %% 615399

    spawn(fun() ->
        io:format("start mysql-connector~n"),
        {ok, M} = mysql_connector_v:start_link([{host, "192.168.26.40"}, {port, 3306}, {user, "root"}, {password, "root"}, {database, "szfy_kaifa"}, {encoding, "utf8mb4"}]),
        {T, _} = timer:tc(fun() ->
            lists:foreach(fun([T]) -> mysql_connector_v:query(<<"select * from szfy_kaifa.", T/binary>>, M) end, mysql_connector_v:query("SHOW TABLES", M))
        end),
        io:format("mysql-connector: ~p~n", [T])
    end),

    ok.


t_f(List) ->

    Start = os:timestamp(),

    ets:safe_fixtable(t, true),
    fill_ets_index(t, ets:first(t)),

    Middle = os:timestamp(),

    fill_list_index(List, 1, []),

    End = os:timestamp(),
    io:format("~p ~p~n", [timer:now_diff(Middle, Start), timer:now_diff(End, Middle)]).

fill_ets_index(Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    ok;
fill_ets_index(Tab, Key) ->
    ets:update_element(Tab, Key, {2, ets:slot(Tab, Key)}),
    fill_ets_index(Tab, ets:next(Tab, Key)).

fill_list_index([], _, List) -> List;
fill_list_index([{K, _} | T], Index, List) ->
    fill_list_index(T, Index + 1, [{K, Index} | List]).

%%%===================================================================
%%% API
%%%===================================================================
t_json() ->
    {ok, Data} = file:read_file("/mnt/d/work/szfy/local/priv/data/create/data_beauty.json"),
    Start = os:timestamp(),
    json:decode(Data),
    Middle = os:timestamp(),
    jsone:decode(Data),
    End = os:timestamp(),
    io:format("json: ~p jsone: ~p diff: ~p~n", [timer:now_diff(Middle, Start), timer:now_diff(End, Middle), timer:now_diff(Middle, Start) - timer:now_diff(End, Middle)]).

%% stack_to_flame.sh < stacks.out > flame.svg
%%%===================================================================
%%% API
%%%===================================================================

%% @doc recompile and reload module
cc() ->
    cc(?MODULE, [debug_info,nowarn_export_all]).
cc(Module) ->
    cc(Module, [debug_info,nowarn_export_all]).
cc(Module, Option) ->
    %% in config dir by default
    cc(Module, "src/", "include/", "beam/", Option).
cc(Module, SrcPath, IncludePath, BeamPath, Option) ->
    c:c(hd(locate(SrcPath, lists:concat([Module, ".erl"]))), [{i, IncludePath}, {outdir, BeamPath} | Option]).

%% @doc find module file from source path
-spec locate(Path :: string(), File :: string()) -> [string()].
locate(Path, File) ->
    {ok, FileList} = file:list_dir_all(Path),
    locate_loop(FileList, Path, File, []).

%% depth first search
locate_loop([], _, _, List) ->
    List;
locate_loop([Name | T], Path, File, List) ->
    SubFile = Path ++ "/" ++ Name,
    case filelib:is_dir(SubFile) of
        true ->
            Result = locate(SubFile, File),
            locate_loop(T, Path, File, List ++ Result);
        false when Name =:= File ->
            locate_loop(T, Path, File, [SubFile | List]);
        _ ->
            locate_loop(T, Path, File, List)
    end.
