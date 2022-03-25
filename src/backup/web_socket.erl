%%%-------------------------------------------------------------------
%%% @doc
%%% module web socket reader
%%% @end
%%%-------------------------------------------------------------------
-module(web_socket).
%% API
-export([handle_upgrade/2, handle_html5_header/2, handle_html5_body_length/2]).
-export([decode/2]).
%% Includes
-include("socket.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc upgrade to web socket
-spec handle_upgrade(State :: #client{}, HttpHead :: #http{}) -> {read, non_neg_integer(), non_neg_integer(), #client{}} | {stop, term(), #client{}}.
handle_upgrade(State, Http) ->
    SecKey = http:get_header_field(<<"Sec-WebSocket-Key">>, Http),
    SecKey1 = http:get_header_field(<<"Sec-WebSocket-Key1">>, Http),
    SecKey2 = http:get_header_field(<<"Sec-WebSocket-Key2">>, Http),
    upgrade(State, Http, SecKey, SecKey1, SecKey2).

%% WebSocket OpCode 定义
%% 0   表示连续消息片断
%% 1   表示文本消息片断
%% 2   表未二进制消息片断
%% 3-7 为将来的非控制消息片断保留的操作码
%% 8   表示连接关闭
%% 9   表示心跳检查的ping
%% A   表示心跳检查的pong
%% B-F 为将来的控制消息片断的保留操作码

%% @doc handle h5 header
-spec handle_html5_header(Data :: binary(), State :: #client{}) -> {read, non_neg_integer(), non_neg_integer(), #client{}} | {stop, term(), #client{}}.
handle_html5_header(<<_Fin:1, _Rsv:3, 8:4, _Msk:1, _Length:7>>, State) ->
    %% quick close client close active
    {stop, {shutdown, closed}, State};
handle_html5_header(<<_Fin:1, _Rsv:3, _OpCode:4, Mask:1, 127:7>>, State) ->
    {read, 8 + Mask * 4, State#client{next_state = handle_html5_body_length, packet = <<127:8>>}};
handle_html5_header(<<_Fin:1, _Rsv:3, _OpCode:4, Mask:1, 126:7>>, State) ->
    {read, 2 + Mask * 4, State#client{next_state = handle_html5_body_length, packet = <<126:8>>}};
handle_html5_header(<<_Fin:1, _Rsv:3, _OpCode:4, Mask:1, Length:7>>, State) ->
    {read, Mask * 4, State#client{next_state = handle_html5_body_length, packet = <<Length:8>>}};
handle_html5_header(Binary, State) ->
    {stop, {h5_header_error, Binary}, State}.

%% @doc handle h5 masking and length
-spec handle_html5_body_length(Data :: binary(), State :: #client{}) -> {read, non_neg_integer(), non_neg_integer(), #client{}} | {stop, term(), #client{}}.
handle_html5_body_length(<<_:64, Masking/binary>>, State = #client{packet = <<127:8>>}) ->
    {read, ?PACKET_HEADER_LENGTH, State#client{next_state = handle_html5_body_head, packet = Masking}};
handle_html5_body_length(<<_:16, Masking/binary>>, State = #client{packet = <<126:8>>}) ->
    {read, ?PACKET_HEADER_LENGTH, State#client{next_state = handle_html5_body_head, packet = Masking}};
handle_html5_body_length(<<Masking/binary>>, State) ->
    {read, ?PACKET_HEADER_LENGTH, State#client{next_state = handle_html5_body_head, packet = Masking}};
handle_html5_body_length(Binary, State) ->
    {stop, {h5_header_length_error, Binary}, State}.

%% @doc web socket decode
-spec decode(Data :: binary(), State :: #client{}) -> {binary(), non_neg_integer(), term()}.
decode(Data, #client{protocol_type = 'HyBi'}) ->
    {decode_frames(Data, <<>>), 0, handle_html5_body};
decode(Data, #client{protocol_type = 'HiXie', masking = Masking}) ->
    {unmask(Data, Masking, <<>>), 2, handle_html5_body}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% web socket upgrade
upgrade(State, Http, <<>>, SecKey1, SecKey2) ->
    %% web socket (ws) HyBi-00
    handshake(State, Http, SecKey1, SecKey2);
upgrade(State, Http, SecKey, <<>>, <<>>) ->
    %% web socket (ws) HiXie-76
    handshake(State, Http, SecKey);
upgrade(State, Http, _, _, _) ->
    %% not web socket packet
    {stop, {no_ws_security_key, Http}, State}.

%% web socket handshake
handshake(State = #client{socket_type = SocketType}, Http, SecKey1, SecKey2) ->
    {_, Scheme} = lists:keyfind(SocketType, 1, [{gen_tcp, <<"ws://">>}, {ssl, <<"wss://">>}]),
    Body = http:get_body(Http),
    Upgrade = http:get_header_field(<<"Upgrade">>, Http),
    Origin = http:get_header_field(<<"Origin">>, Http),
    Host = http:get_header_field(<<"Host">>, Http),
    Uri = http:get_uri(Http),
    Integer1 = erlang:binary_to_integer(<< <<D:8>> || <<D:8>> <= SecKey1, $0 =< D, D =< $9 >>),
    Integer2 = erlang:binary_to_integer(<< <<D:8>> || <<D:8>> <= SecKey2, $0 =< D, D =< $9 >>),
    Blank1 = erlang:byte_size(<< <<S:8>> || <<S:8>> <= SecKey1, S =:= $  >>),
    Blank2 = erlang:byte_size(<< <<S:8>> || <<S:8>> <= SecKey2, S =:= $  >>),
    %% handshake response
    Handshake = [
        <<"HTTP/1.1 101 WebSocket Protocol Handshake\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Origin: ">>, Origin, <<"\r\n">>,
        <<"Sec-WebSocket-Location: ">>, Scheme, Host, Uri, <<"\r\n\r\n">>,
        erlang:md5(<<(Integer1 div Blank1):4/big-unsigned-integer-unit:8, (Integer2 div Blank2):4/big-unsigned-integer-unit:8, Body/binary>>)
    ],
    sender:send(State, list_to_binary(Handshake)),
    %% {read, 0, State#client{state = handle_html5_body, protocol_type = 'HyBi'}}.
    State#client{next_state = handle_web_socket_body, protocol_type = 'HyBi'}.
handshake(State, Http, SecKey) ->
    Hash = crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    Upgrade = http:get_header_field(<<"Upgrade">>, Http),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Binary)),
    State#client{next_state = handle_web_socket_header, protocol_type = 'HiXie'}.
    %% {read, 2, State#client{state = handle_html5_header, protocol_type = 'HiXie'}}.

%% HyBi decode frames
decode_frames(<<>>, Frames) ->
    Frames;
decode_frames(<<0, T/binary>>, Frames) ->
    {Frame, Rest} = parse_frame(T, <<>>),
    decode_frames(Rest, <<Frames/binary, Frame/binary>>).
parse_frame(<<255, Rest/binary>>, Buffer) ->
    {Buffer, Rest};
parse_frame(<<H, T/binary>>, Buffer) ->
    parse_frame(T, <<Buffer/binary, H>>).

%% HiXie unmask
unmask(<<>>, _, Acc) ->
    Acc;
unmask(<<A:8>>, <<MA:8, _/binary>>, Acc) ->
    <<Acc/binary, (MA bxor A)>>;
unmask(<<A:8, B:8>>, <<MA:8, MB:8, _/binary>>, Acc) ->
    <<Acc/binary, (MA bxor A), (MB bxor B)>>;
unmask(<<A:8, B:8, C:8>>, <<MA:8, MB:8, MC:8, _:8>>, Acc) ->
    <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
unmask(<<A:8, B:8, C:8, D:8, Rest/binary>>, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    NewAcc = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
    unmask(Rest, Masking, NewAcc).
