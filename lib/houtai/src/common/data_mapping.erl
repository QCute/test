%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. ÆßÔÂ 2018 14:02
%%%-------------------------------------------------------------------
-module(data_mapping).
-author("Administrator").

%% API
-compile(export_all).


get_mysql_pool_name(Tag) when is_atom(Tag) ->
    get_mysql_pool_name(atom_to_list(Tag));
get_mysql_pool_name(Tag) when is_list(Tag) ->
    list_to_atom("mysql_pool_" ++ Tag).


get_record_server_name(Tag) when is_atom(Tag) ->
    get_record_server_name(atom_to_list(Tag));
get_record_server_name(Tag) when is_list(Tag) ->
    list_to_atom("record_server_" ++ Tag).


get_tracer_server_name(Tag) when is_atom(Tag) ->
    get_tracer_server_name(atom_to_list(Tag));
get_tracer_server_name(Tag) when is_list(Tag) ->
    list_to_atom("tracer_server_" ++ Tag).


get_lock_server_name(Tag) when is_atom(Tag) ->
    get_lock_server_name(atom_to_list(Tag));
get_lock_server_name(Tag) when is_list(Tag) ->
    list_to_atom("lock_server_" ++ Tag).
