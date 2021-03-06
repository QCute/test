%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol maker
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker_backup).
-include("serialize.hrl").
-export([start/1]).
%%%===================================================================
%%% API
%%%===================================================================
start(List) ->
    maker:start(fun parse/2, List).

%%%===================================================================
%%% parse
%%%===================================================================
parse(_, {_, #protocol{io = IO, includes = Includes, erl = File}}) ->
    [Module | _] = string:tokens(hd(lists:reverse(string:tokens(File, "/"))), "."),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    Code = collect_code(IO, [], []),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
    [{"(?s).*", Head ++ Include ++ Code}].

%% collect code
collect_code([], ReadList, WriteList) ->
    ReadMatch = "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n",
    WriteMatch = "write(Code, Content) ->\n    {error, Code, Content}.\n",
    lists:concat(["\n\n", lists:reverse(ReadList), ReadMatch, "\n\n", lists:reverse(WriteList), WriteMatch]);
collect_code([#io{read = Read, write = Write, name = Name} | T], ReadList, WriteList) ->
    ReadCode = format_read(Name, Read),
    erase(),
    WriteCode = format_write(Name, Write),
    erase(),
    collect_code(T, [ReadCode | ReadList], [WriteCode | WriteList]).

%%====================================================================
%% read code part
%%====================================================================
format_read(Code, []) ->
    {"read(" ++ type:to_list(Code) ++ ", <<>>) ->\n    {ok, []};\n\n", [], []};
format_read(Code, List) ->
    {M, E, P, N, T} = format_read(List, [], [], [], [], []),
    {lists:concat(["read(", Code, ", ", P, ") ->\n"]) ++ E ++ lists:concat(["    {ok, [", M, "]};\n\n"]), N, T}.
format_read([], Match, Expression, Pack, Name, Type) ->
    {string:join(lists:reverse(Match), ", "), [X ++ ",\n" || X = [_ | _] <- lists:reverse(Expression)], lists:concat(["<<", string:join(lists:reverse(Pack), ", "), ">>"]), Name, Type};
format_read([H | T], Match, Expression, Pack, Name, Type) ->
    {MatchParam, PackInfo, NameList, TypeList} = format_read_unit(H, undefined),
    case is_record(H, list) of
        true ->
            ListLength = PackInfo ++ "Length",
            case re:run(MatchParam, "/binary", [global, {capture, all, list}]) =/= nomatch of
                true ->
                    %% with string can only use binary
                    ListMatch = format("Binary:~s/binary", [ListLength]);
                _ ->
                    %% without string calc binary unit
                    {match, Result} = re:run(MatchParam, "(?<=:)\\d+", [global, {capture, all, list}]),
                    ListMatch = format("Binary:~s/binary-unit:~p", [ListLength, lists:sum([list_to_integer(X) || [X] <- Result])])
            end,
            %% unpack binary to list
            E = lists:concat(["    ", PackInfo, " = [", MatchParam, "]"]),
            %% fill binary match param, list length use 16 bit constant
            P = ListLength ++ ":16, " ++ PackInfo ++ ListMatch,
            format_read(T, [PackInfo | Match], [E | Expression], [P | Pack], [NameList | Name], [TypeList | Type]);
        _ ->
            format_read(T, [MatchParam | Match], Expression, [PackInfo | Pack], [NameList | Name], [TypeList | Type])
    end.

%%====================================================================
%% write code part
%%====================================================================
format_write(Code, []) ->
    {"write(" ++ type:to_list(Code) ++ ", []) ->\n    {ok, protocol:pack(" ++ type:to_list(Code) ++ ", <<>>)};\n\n", [], []};
format_write(Code, List) ->
    {M, E, P, N, T} = format_write(List, [], [], [], [], []),
    {lists:concat(["write(", Code, ", [", M, "]) ->\n"]) ++ E ++ lists:concat(["    {ok, protocol:pack(", Code, ", ", P, ")};\n\n"]), N, T}.
format_write([], Match, Expression, Pack, Name, Type) ->
    {string:join(lists:reverse(Match), ", "), [X ++ ",\n" || X = [_ | _] <- lists:reverse(Expression)], lists:concat(["<<", string:join(lists:reverse(Pack), ", "), ">>"]), Name, Type};
format_write([H | T], Match, Expression, Pack, Name, Type) ->
    {MatchParam, PackInfo, NameList, TypeList} = format_write_unit(H, undefined),
    io:format("Name:~p~n", [NameList]),
    io:format("Type:~p~n", [TypeList]),

    case is_record(H, ets) of
        true ->
            %% pack list data do in function expression
            E = lists:concat(["    ", MatchParam, "Binary = ", PackInfo, ""]),
            P = MatchParam ++ "Binary/binary",
            format_write(T, [MatchParam | Match], [E | Expression], [P | Pack], [NameList | Name], [TypeList | Type]);
        false when is_record(H, list) ->
            %% pack list data do in function expression
            E = lists:concat(["    ", MatchParam, "Binary = ", PackInfo, ""]),
            P = MatchParam ++ "Binary/binary",
            format_write(T, [MatchParam | Match], [E | Expression], [P | Pack], [NameList | Name], [TypeList | Type]);
        false when is_tuple(H) ->
            %% pack list data do in function expression
            E = lists:concat(["    ", MatchParam, "Binary = ", PackInfo, ""]),
            P = MatchParam ++ "Binary/binary",
            format_write(T, [MatchParam | Match], [E | Expression], [P | Pack], [NameList | Name], [TypeList | Type]);
        _ ->
            format_write(T, [MatchParam | Match], Expression, [PackInfo | Pack], [NameList | Name], [TypeList | Type])
    end.

%%====================================================================
%% read unit part
%%====================================================================
format_read_unit(#u8{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:8", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u8};
format_read_unit(#u16{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:16", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u16};
format_read_unit(#u32{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:32", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u32};
format_read_unit(#u64{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:64", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u64};
format_read_unit(#u128{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:128", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u128};
format_read_unit(#bst{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    {Param, Pack, lower_hump(HumpName), string};
format_read_unit(#str{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("binary_to_list(~s)", [HumpName]),
    Pack = format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    {Param, Pack, lower_hump(HumpName), string};

%% structure unit
format_read_unit(#list{name = Name, explain = Explain}, Extra) ->
    %% hump name is unpack bit variable bind
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack, ListName, ListType} = format_read_unit(Explain, Extra),
    %% format list pack info
    Param = format("~s || <<~s>> <= ~s", [ListParam, ListPack, Hump ++ "Binary"]),
    {Param, Hump, ListName, ListType};
format_read_unit(Record, _) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:get(Tag),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [{format_read_unit(Explain, Name), Name} || {Explain, Name} <- ZipList, is_unit(Explain)],
    %% format function match param
    Param = lists:concat(["#", Tag, "{", string:join([format("~s = ~s", [Name, MatchParam]) || {{MatchParam = [_ | _], _, _, _}, Name} <- List], ", "), "}"]),
    %% format pack info
    Pack = string:join([PackInfo || {{_, PackInfo = [_ | _], _, _}, _} <- List], ", "),
    {Param, Pack, [X || {{_, _, X, _}, _} <- List], [X || {{_, _, _, X}, _} <- List]};
format_read_unit(Tuple, Extra) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% make tuple name list
    NameList = [format("~s", [choose_name(lists:concat([Extra, No]))]) || No <- lists:seq(1, tuple_size(Tuple))],
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Tuple), NameList),
    %% format per unit
    List = [format_read_unit(Explain, Name) || {Explain, Name} <- ZipList],
    %% format function match param
    case string:join([MatchParam || {MatchParam = [_ | _], _, _, _} <- List], ", ") of
        [] ->
            Param = [];
        MatchParam ->
            Param = lists:concat(["{", MatchParam, "}"])
    end,
    %% format pack info
    Pack = string:join([PackInfo || {_, PackInfo = [_ | _], _, _} <- List], ", "),
    {Param, Pack, [X || {_, _, X = [_ | _], _} <- List], [X || {_, _, _, X} <- List, X =/= []]};

format_read_unit(_, _) ->
    {"", "", "", ""}.

%%====================================================================
%% write unit part
%%====================================================================
format_write_unit(#zero{}, _) ->
    Param = format("_", []),
    Pack = format("", []),
    {Param, Pack, [], []};
format_write_unit(#u8{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:8", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u8};
format_write_unit(#u16{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:16", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u16};
format_write_unit(#u32{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:32", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u32};
format_write_unit(#u64{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:64", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u64};
format_write_unit(#u128{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:128", [HumpName]),
    {Param, Pack, lower_hump(HumpName), u128};
format_write_unit(#bst{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("(byte_size(~s)):16, (~s)/binary", [HumpName, HumpName]),
    {Param, Pack, lower_hump(HumpName), string};
format_write_unit(#str{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("(length(~s)):16, (iolist_to_binary(~s))/binary", [HumpName, HumpName]),
    {Param, Pack, lower_hump(HumpName), string};

%% structure unit
format_write_unit(#ets{name = Name, explain = Explain}, Extra) ->
    %% auto make undefined name
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack, ListName, ListType} = format_write_unit(Explain, Extra),
    %% format list pack info
    Pack = format("protocol:write_ets(fun([~s]) -> <<~s>> end, ~s)", [ListParam, ListPack, Hump]),
    {Hump, Pack, ListName, ListType};
format_write_unit(#list{name = Name, explain = Explain}, Extra) ->
    %% auto make undefined name
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack, ListName, ListType} = format_write_unit(Explain, Extra),
    %% format list pack info
    Pack = format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>/binary", [Hump, ListPack, ListParam, Hump]),
    {Hump, Pack, ListName, ListType};
format_write_unit(Record, _) when is_tuple(Record) andalso tuple_size(Record) > 0 andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:get(Tag),
    %% throw error when beam abstract code empty
    NameList =:= [] andalso erlang:error(need_to_update_beam_abstract_code),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [{format_write_unit(Explain, Name), Name} || {Explain, Name} <- ZipList, is_unit(Explain)],
    %% format function match param
    Param = lists:concat(["#", Tag, "{", string:join([format("~s = ~s", [Name, MatchParam]) || {{MatchParam = [_ | _], _, _, _}, Name} <- List], ", "), "}"]),
    %% format pack info
    Pack = string:join([PackInfo || {{_, PackInfo = [_ | _], _, _}, _} <- List], ", "),
    {Param, Pack, [X || {{_, _, X, _}, _} <- List], [X || {{_, _, _, X}, _} <- List]};
format_write_unit(Tuple, Extra) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% make tuple name list
    NameList = [format("~s", [choose_name(lists:concat([Extra, No]))]) || No <- lists:seq(1, tuple_size(Tuple))],
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Tuple), NameList),
    %% format per unit
    List = [format_write_unit(Explain, Name) || {Explain, Name} <- ZipList],
    %% format function match param
    case string:join([MatchParam || {MatchParam = [_ | _], _} <- List], ", ") of
        [] ->
            Param = [];
        MatchParam ->
            Param = lists:concat(["{", MatchParam, "}"])
    end,
    %% format pack info
    Pack = string:join([PackInfo || {_, PackInfo = [_ | _]} <- List], ", "),
    {Param, Pack, [X || {_, _, X = [_ | _], _} <- List], [X || {_, _, _, X} <- List, X =/= []]};

format_write_unit(_, _) ->
    {"", "", "", ""}.


%%====================================================================
%% named tool
%%====================================================================
%% auto make name
choose_name(Name) ->
    choose_name(Name, undefined).
choose_name(undefined, undefined) ->
    case get('name') of
        undefined ->
            Name = lists:concat([undefined, 1]),
            put('name', 1),
            maker:hump(Name);
        AI ->
            Name = lists:concat([undefined, AI + 1]),
            put('name', AI + 1),
            maker:hump(Name)
    end;
choose_name(undefined, Outer) ->
    maker:hump(Outer);
choose_name(Inner, _) ->
    maker:hump(Inner).

%% IsLower => isLower
lower_hump([H | T]) ->
    [string:to_lower(H) | T].

%% record name
%%choose_record_name(#u8{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(#u16{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(#u32{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(#u64{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(#u128{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(#str{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(#bst{name = Name}) ->
%%    choose_name(Name);
%%choose_record_name(Record) ->
%%    maker:hump(element(1, Record)).
%%====================================================================
%% common tool
%%====================================================================
%% format
format(F, A) ->
    binary_to_list(list_to_binary(io_lib:format(F, A))).

%% is bit unit
is_unit(Tag) when is_atom(Tag) ->
    false;
is_unit(Tag) when is_integer(Tag) ->
    false;
is_unit(Tag) when is_binary(Tag) ->
    false;
is_unit(Tag) when is_list(Tag) ->
    true;
is_unit(Tag) when is_tuple(Tag) andalso is_atom(element(1, Tag)) ->
    true;
is_unit(Tag) when is_tuple(Tag) ->
    true;
is_unit(_) ->
    false.
