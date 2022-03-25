%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 五月 2018 15:29
%%%-------------------------------------------------------------------
-module(record_server_util).
-author("Administrator").

%% API
-export([try_print_record/2, print_record/2, try_print_string_record/2]).

try_print_string_record({ServerNode,FetchNameModule}, Data) when is_binary(Data) orelse is_list(Data)andalso is_atom(FetchNameModule)->
    try
        Data1 = re:replace(Data, "#\\w+<\\d+(\\.\\d+?)+>", "\"&\"", [global,{return, list}]),
        Data2 = util:eval_string(Data1 ++ [$.]),
        try_print_record({ServerNode,FetchNameModule}, Data2)
    catch
        _:_ ->
            io_lib:format("try_print_string_record bad data~n~p", [Data])
    end;
try_print_string_record(_, Data) ->
    io_lib:format("try_print_string_record bad data~n~p", [Data]).

try_print_record({ServerNode,FetchNameModule}, Data) when is_binary(Data)  andalso is_atom(FetchNameModule)->
    case catch binary_to_list(Data) of
        Data1 when is_list(Data1) ->
            try_print_record({ServerNode,FetchNameModule}, Data1);
        _ ->
            io_lib:format("try_print_record bad data~n~p", [Data])
    end;
try_print_record({ServerNode,FetchNameModule}, Data) when is_tuple(Data) andalso element(1, Data) =:= dict  andalso is_atom(FetchNameModule)->
    ValueList = dict:fold(fun(_K, V, L) -> [V | L] end, [], Data),
    try_print_record({ServerNode,FetchNameModule}, ValueList);
try_print_record({ServerNode,FetchNameModule}, Data) when is_tuple(Data) andalso is_atom(FetchNameModule) ->
    try_print_record({ServerNode,FetchNameModule}, [Data]);
try_print_record({ServerNode,FetchNameModule}, Data) when is_list(Data) andalso is_atom(FetchNameModule) ->
    DataLength = length(Data),
    case catch tuple_to_list(lists:nth(1, Data)) of
        MaybeRecord when is_list(MaybeRecord) ->
            RecordLength = length(MaybeRecord),
            case RecordLength > 0 of
                true ->
                    RecordServerName = data_mapping:get_record_server_name(ServerNode),
                    RecordName = hd(MaybeRecord),
                    case gen_server:call(RecordServerName, {get_record_info, RecordName}) of
                        {ok, RecordInfo} when length(RecordInfo) =:= RecordLength ->
                            {ok, TableAonnate} = make_table_aonnate(RecordInfo),
                            {ok, Columns, Length} = make_table_head(RecordInfo),
                            FieldsName = list_to_tuple(
                                lists:map(fun([_, FieldName]) ->
                                    FieldName
                                end, tuple_to_list(Columns))
                            ),
                            {Columns1, Length1} = lists:foldl(
                                fun(RecordData, {Result5, Length5}) when is_tuple(RecordData) ->
                                    RecordData1 = tuple_to_list(RecordData),
                                    case length(RecordData1) =:= RecordLength andalso hd(RecordData1) =:= RecordName of
                                        true ->
                                            [_ | RecordData2] = RecordData1,
                                            {Result7, Length7, _} = lists:foldl(
                                                fun(Field1, {Result6, Length6, N1}) ->
                                                    FieldName1 =element(N1, FieldsName),
                                                    FieldData = make_field_data(Field1,FieldName1),
                                                    Name =
                                                        case erlang:function_exported(FetchNameModule,record_server_field_try_fetch_name,3) of
                                                            true ->
                                                                FetchNameModule:record_server_field_try_fetch_name(RecordName, FieldName1,Field1);
                                                            false ->
                                                                ""
                                                        end,
                                                    make_column({FieldData ++ Name, Result6, Length6, N1})
                                                end, {Result5, Length5, 1}, RecordData2),
                                            {Result7, Length7};
                                        false ->
                                            {Result5, Length5}
                                    end;
                                    (_, {Result5, Length5}) ->
                                        {Result5, Length5}
                                end, {Columns, Length}, Data),
                            {ok, Body} = make_body(Columns1, Length1, RecordInfo, DataLength),
                            lists:flatten([TableAonnate | Body]);
                        _ -> io_lib:format("try_print_record bad data~n~p", [Data])
                    end;
                _ -> io_lib:format("try_print_record bad data~n~p", [Data])
            end;
        _ -> io_lib:format("try_print_record bad data~n~p", [Data])
    end;
try_print_record(_, Data) ->
    io_lib:format("try_print_record bad data~n~p", [Data]).

get_string_length(String) ->
    Utf8 = unicode:characters_to_list_int(list_to_binary(String), utf8),
    lists:foldl(fun(Char, Length) ->
        case Char > 177 of
            true -> Length + 2;
            false -> Length + 1
        end
    end, 0, Utf8).

fix_string_length(String, Length) when is_integer(Length) andalso Length > 0 ->
    Fix = Length - get_string_length(String),
    case Fix > 0 of
        true ->
            lists:reverse(lists:foldl(
                fun(_, L) -> [$  | L]
                end, [], lists:seq(1, Fix)) ++ lists:reverse(String));
        false ->
            String
    end;
fix_string_length(String, _) ->
    String.

make_column({String, Result, Length, N}) ->
    SL = get_string_length(String),
    Fix = SL - element(N, Length),
    case Fix > 0 of
        true ->
            NewResult = setelement(N, Result,
                %%                 [String | lists:reverse(lists:foldl(
                %%                     fun(RS, NR) ->
                %%                         [fix_string_length(RS, Fix) | NR]
                %%                     end, [], element(N, Result)))]
                [String | element(N, Result)]
            ),
            NewLength = setelement(N, Length, SL),
            {NewResult, NewLength, N + 1};
        false ->
            {setelement(N, Result, [String | element(N, Result)]), Length, N + 1}
    end.

print_record(ServerNode, RecordName) when is_list(RecordName) orelse is_binary(RecordName) orelse is_atom(RecordName) ->
    case record_server:get_record_info(ServerNode,  RecordName) of
        {ok, RecordInfo} ->
            {ok, TableAonnate} = make_table_aonnate(RecordInfo),
            {ok, Columns, Length} = make_table_head(RecordInfo),
            {ok, Body} = make_body(Columns, Length, RecordInfo, 0),
            lists:flatten([TableAonnate | Body]);
        error ->
            io_lib:format("print_record unknow record ~p", [RecordName]);
        Err ->
            houtai_error_logger:error_report("unknow error in print_record~n~p~n",[Err]),
            io_lib:format("print_record err", [])
    end;
print_record(_ServerNode,RecordName) ->
    io_lib:format("print_record bad data ~p", [RecordName]).

make_table_aonnate(RecordInfo) ->
    [{RecordName1, RecordNameAonnate} | _] = RecordInfo,
    Head1 = binary_to_list(<<"记录名 : ">>),
    Head2 = binary_to_list(<<",注释 : ">>),
    Head = Head1 ++ RecordName1 ++ Head2 ++ RecordNameAonnate ++ "\n",
    {ok, Head}.

make_table_head(RecordInfo) ->
    [_ | RecordInfo1] = RecordInfo,
    RecordLength = length(RecordInfo),
    Result = list_to_tuple(lists:map(fun(_) -> [] end, lists:seq(1, RecordLength - 1))),
    Length = list_to_tuple(lists:map(fun(_) -> 0 end, lists:seq(1, RecordLength - 1))),
    {Result4, Length4, _} = lists:foldl(
        fun({Name, Aonnate}, {Result1, Length1, N}) ->
            NameLength = get_string_length(Name),
            Result2 = setelement(N, Result1, [Name]),
            Length2 = setelement(N, Length1, NameLength),
            {Result3, Length3, _} = make_column({Aonnate, Result2, Length2, N}),
            {Result3, Length3, N + 1}
        end, {Result, Length, 1}, RecordInfo1),
    {ok, Result4, Length4}.

make_body(Result, Length, RecordInfo, DataLength) ->
    RecordLength = length(RecordInfo),
    Body = lists:foldl(fun(N2, Body1) ->
        case DataLength > 1 of
            true ->
                {Row1, _} = lists:foldl(fun(N3, {Row, SumLen}) ->
                    case SumLen > 200 of
                        true ->
                            {[$|, $ , fix_string_length(lists:nth(N2, element(N3, Result)), element(N3, Length)), $|, $ , fix_string_length(lists:nth(N2, element(1, Result)), element(1, Length)) | Row], 0};
                        false ->
                            {[$|, $ , fix_string_length(lists:nth(N2, element(N3, Result)), element(N3, Length)) | Row], SumLen + element(N3, Length)}
                    end
                end, {[], 0}, lists:seq(1, RecordLength - 1, 1));
            false ->
                Row1 = lists:foldl(fun(N3, Row) ->
                    [$|, $ , fix_string_length(lists:nth(N2, element(N3, Result)), element(N3, Length)) | Row]
                end, [], lists:seq(1, RecordLength - 1, 1))
        end,
        [lists:reverse(Row1), "\n" | Body1]
    end, [], lists:seq(1, DataLength + 2)),
    {ok, Body}.

%% 处理[]
make_field_data([],_) ->
    "[]";
%% 尝试转换成文字
make_field_data(Field,_) when is_binary(Field) ->
    binary_to_list(Field);
make_field_data(Field,_) when is_list(Field) ->
    %% 全是ASCII或utf8的话直接打印
    case [E || E <- Field, not(0 =< E andalso E =< 255)] of
        [] -> Field;
        _ -> lists:flatten(io_lib:format("~w", [Field]))
    end;
%% 尝试转换成时间
make_field_data(Field,FieldName) when is_integer(Field) andalso Field >= 1514736000andalso Field=< 9999999999->
    case re:run(FieldName,"time") of
        nomatch ->
            lists:flatten(io_lib:format("~w", [Field]));
        _ ->
            {{Year,Month,Day},{Hour,Min,Sec}} = util:unixtime_to_date(Field),
            lists:flatten(io_lib:format("~p-~p-~p ~p:~p:~p", [Year,Month,Day,Hour,Min,Sec]))
    end;
make_field_data(Field,_) ->
    lists:flatten(io_lib:format("~w", [Field])).

