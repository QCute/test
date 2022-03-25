%%%-------------------------------------------------------------------
%%% @doc
%%% misc code
%%% @end
%%%-------------------------------------------------------------------
-module(misc).
-compile(nowarn_deprecated_function).
-compile(nowarn_export_all).
-compile(export_all).
%% Includes

%% +-----------------------------------------------------------------------------------------+
%% | process flag and exit operation                                                         |
%% | trap_exit flag will converted exit message to {'EXIT', From, Reason}                    |
%% +-----------+--------+--------------------------------------------------------------------+
%% | trap_exit | signal | action                                                             |
%% +-----------+--------+--------------------------------------------------------------------+
%% | true      | kill   | Die: Broadcast the exit signal killed to the linked processes.     |
%% | true      | X      | Add {'EXIT', Pid, X} to the mailbox.                               |
%% | false     | normal | Continue: Do-nothing signal vanishes                               |
%% | false     | kill   | Die: Broadcast the exit signal killed to the linked processes.     |
%% | false     | X      | Die: Broadcast the exit signal X to the linked processes           |
%% +-----------+--------+--------------------------------------------------------------------+

f() ->
    ok.
