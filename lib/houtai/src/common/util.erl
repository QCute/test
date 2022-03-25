%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 七月 2018 10:21
%%%-------------------------------------------------------------------
-module(util).
-author("Administrator").

%% API
-compile(export_all).

%% 执行字符串
eval_string(Msg) when is_binary(Msg) ->
    eval_string(binary_to_list(Msg));
eval_string(Msg) when is_list(Msg) ->
    {ok, Scan, _} = erl_scan:string(Msg),
    {ok, Exprs} = erl_parse:parse_exprs(Scan),
    {value, Value, _} = erl_eval:exprs(Exprs, orddict:new()),
    Value;
eval_string(_) ->
    err.


%% 获取网络时间,返回格式 {{2018,1,1},{0,0,0}}
get_net_time() ->
    inets:start(),
    try get_net_time1() of
        Return when is_tuple(Return) -> Return
    catch _:_ ->
        get_net_time()
    end.
get_net_time1() ->
    {ok, {_HttpVersion, Heads, _Body}} = httpc:request(get, {"http://www.baidu.com", []}, [], []),
    {"date", Date} = lists:keyfind("date", 1, Heads),
    [_Week, Day1, Month1, Year1, Time, _Standard] = binary:split(list_to_binary(Date), <<" ">>, [global]),
    [Hour1, Min1, Second1] = binary:split(Time, <<":">>, [global]),
    Day = binary_to_integer(Day1),
    Year = binary_to_integer(Year1),
    Hour = binary_to_integer(Hour1),
    Min = binary_to_integer(Min1),
    Second = binary_to_integer(Second1),
    Month = case Month1 of
                <<"Jan">> -> 1;
                <<"Feb">> -> 2;
                <<"Mar">> -> 3;
                <<"Apr">> -> 4;
                <<"May">> -> 5;
                <<"Jun">> -> 6;
                <<"Jul">> -> 7;
                <<"Aug">> -> 8;
                <<"Sep">> -> 9;
                <<"Oct">> -> 10;
                <<"Nov">> -> 11;
                <<"Dec">> -> 12
            end,
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}}) + Hour * 3600 + Min * 60 + Second + 8 * 3600).

%% 同步cmd命令
synchronization_cmd(Format, FormatArgs) ->
    synchronization_cmd(Format, FormatArgs, []).
synchronization_cmd(Format, FormatArgs, Arg) ->
    CmdHead = case os:type() of
                  {unix, _} ->
                      "";
                  {win32, _} ->
                      "cmd /c "
              end,
    case catch lists:flatten(io_lib:format(CmdHead ++ Format, FormatArgs)) of
        String when is_list(String) ->
            Port = erlang:open_port({spawn, String}, [exit_status]++Arg),
            synchronization_cmd_loop(Port,String);
        _ ->
            houtai_error_logger:error_report("bad cmd ~n~p~n~p", [Format, FormatArgs]),
            bad_cmd
    end.
synchronization_cmd_loop(Port, String) ->
    receive
        {Port, {exit_status, _ExitStatus}} ->ok;
        _ ->
            synchronization_cmd_loop(Port, String)
    after
        5 * 60 * 1000 ->
            catch erlang:port_close(Port),
            houtai_error_logger:error_report("cmd timeout~n~s", [String])
    end.

%% 执行cmd无返回
no_return_cmd(Format,FormatArgs)->
    no_return_cmd(Format,FormatArgs,[]).
no_return_cmd(Format,FormatArgs,Arg) ->
    CmdHead = case os:type() of
                  {unix, _} ->
                      "";
                  {win32, _} ->
                      "cmd /c "
              end,
    case catch lists:flatten(io_lib:format(CmdHead ++ Format, FormatArgs)) of
        String when is_list(String) ->
            Port = erlang:open_port({spawn, String}, Arg),
            erlang:port_close(Port);
        _ ->
            houtai_error_logger:error_report("bad cmd ~n~s~n~p", [Format, FormatArgs]),
            bad_cmd
    end.

%% unix时间转换成本地时间,返回格式 {{2018,1,1},{0,0,0}}
unixtime_to_date(UnixTime) when is_integer(UnixTime)->
    T1 = UnixTime div 1000 * 1000,
    T2 =UnixTime- T1 * 1000 * 1000 ,
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_local_time({T1,T2,0}),
    {{Year,Month,Day},{Hour,Min,Sec}}.


%% 获取数据
get_data(Data, DataTypeList) ->
    get_data(Data, DataTypeList, <<";;;">>).
get_data(Data, DataTypeList, Split) when is_binary(Data) ->
    DataList = binary:split(Data, Split, [global]),
    case is_list(DataTypeList) andalso length(DataList) =:= length(DataTypeList) of
        true ->
            get_data1(DataList, DataTypeList, []);
        false ->
            bad_argument
    end.
get_data1([], [], Result) ->
    lists:reverse(Result);
get_data1([H1 | T1], [H2 | T2], Result) ->
    try
        begin
            Result1 = case H2 of
                          int ->
                              binary_to_integer(H1);
                          atom ->
                              binary_to_atom(H1, utf8);
                          binary ->
                              H1;
                          list ->
                              binary_to_list(H1);
                          term ->
                              util:eval_string(binary_to_list(H1) ++ [$.])
                      end,
            get_data1(T1, T2, [Result1 | Result])
        end
    catch
        _:_ ->
            bad_argument
    end.