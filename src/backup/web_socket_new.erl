%%%-------------------------------------------------------------------
%%% @doc
%%% module web socket
%%% @end
%%%-------------------------------------------------------------------
-module(web_socket_new).
%% API
-export([handle_upgrade/2]).
-export([unmask/3, decode_frames/2]).
%% Includes
-include("socket.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc upgrade to web socket
-spec handle_upgrade(HttpHead :: #http{}, State :: #client{}) -> NewState :: #client{} | {stop, term(), NewState :: #client{}}.
handle_upgrade(Http, State) ->
    SecKey = http:get_header_field(<<"Sec-WebSocket-Key">>, Http),
    SecKey1 = http:get_header_field(<<"Sec-WebSocket-Key1">>, Http),
    SecKey2 = http:get_header_field(<<"Sec-WebSocket-Key2">>, Http),
    upgrade(State, Http, SecKey, SecKey1, SecKey2).

%% @doc unmask (Draft-HiXie-76)
-spec unmask(Payload :: binary(), Masking :: binary(), Acc :: binary()) -> binary().
unmask(<<>>, _Masking, Acc) ->
    Acc;
unmask(PayLoad, <<>>, _) ->
    PayLoad;
unmask(<<A:8>>, <<MA:8, _MB:8, _MC:8, _MD:8>>, Acc) ->
    <<Acc/binary, (MA bxor A)>>;
unmask(<<A:8, B:8>>, <<MA:8, MB:8, _MC:8, _MD:8>>, Acc) ->
    <<Acc/binary, (MA bxor A), (MB bxor B)>>;
unmask(<<A:8, B:8, C:8>>, <<MA:8, MB:8, MC:8, _MD:8>>, Acc) ->
    <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
unmask(<<A:8, B:8, C:8, D:8, Rest/binary>>, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    NewAcc = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
    unmask(Rest, Masking, NewAcc).

%% decode frames (Draft-HyBi-00)
-spec decode_frames(Frames :: binary(), Acc :: binary()) -> binary().
decode_frames(<<>>, Acc) ->
    Acc;
decode_frames(<<0, T/binary>>, Acc) ->
    {Frame, Rest} = parse_frame(T, <<>>),
    decode_frames(Rest, <<Acc/binary, Frame/binary>>).
parse_frame(<<>>, Acc) ->
    {Acc, <<>>};
parse_frame(<<255, Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_frame(<<H, T/binary>>, Acc) ->
    parse_frame(T, <<Acc/binary, H>>).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% web socket upgrade
upgrade(State, Http, SecKey, <<>>, <<>>) ->
    %% web socket (ws)
    handshake(State, Http, SecKey);
upgrade(State, Http, <<>>, SecKey1, SecKey2) ->
    %% web secure socket (wss)
    handshake(State, Http, SecKey1, SecKey2);
upgrade(State, Http, _, _, _) ->
    %% not web socket packet
    {stop, {no_web_socket_security_key, Http}, State}.

%% web socket handshake
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
    State#client{state = handle_web_socket_packet, protocol_type = 'HiXie'}.
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
    State#client{state = handle_web_socket_packet_old, protocol_type = 'HyBi'}.

