%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. ÆßÔÂ 2018 15:49
%%%-------------------------------------------------------------------
-module(houtai_handler).
-author("Administrator").

%% API
-export([]).

-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown}
| {normal, timeout}
| {error, closed}
| {remote, closed}
| {remote, cowboy_websocket:close_code(), binary()}
| {error, badencoding}
| {error, badframe}
| {error, atom()}.

-callback websocket_init(atom(), Req, opts())
        -> {ok, Req, state()}
    | {ok, Req, state(), hibernate}
    | {ok, Req, state(), timeout()}
    | {ok, Req, state(), timeout(), hibernate}
    | {shutdown, Req}
    when Req::cowboy_req:req().
-callback websocket_handle({text | binary | ping | pong, binary()}, Req, State)
        -> {ok, Req, State}
    | {ok, Req, State, hibernate}
    | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
    | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
    | {shutdown, Req, State}
    when Req::cowboy_req:req(), State::state().
-callback websocket_info(any(), Req, State)
        -> {ok, Req, State}
    | {ok, Req, State, hibernate}
    | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
    | {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
    | {shutdown, Req, State}
    when Req::cowboy_req:req(), State::state().
-callback websocket_terminate(terminate_reason(), cowboy_req:req(), state())
        -> ok.


-callback record_server_field_try_fetch_name(RecordName::term(), FiledName::term(),FieldData::term())
        -> list().