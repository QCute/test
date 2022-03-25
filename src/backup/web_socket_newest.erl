%%%-------------------------------------------------------------------
%%% @doc
%%% tcp receiver
%%% @end
%%%-------------------------------------------------------------------
-module(receiver).
-compile({inline, [decode_header/2, receive_data/2]}).
%% API
-export([start/2]).
%% gen_server callbacks
-export([init/1]).
%% Includes
-include_lib("ssl/src/ssl_api.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start(SocketType :: gen_tcp | ssl, Socket :: gen_tcp:socket() | ssl:sslsocket()) -> {ok, pid()} | {error, term()}.
start(SocketType, Socket) ->
    %% do not mirror by the net supervisor
    spawn_link(?MODULE, init, [#client{socket_type = SocketType, socket = Socket}]).

%% handle tcp/http stream
-spec init(State :: #client{}) -> no_return().
init(State) ->
    %% wait for
    _ = receive start -> ok after ?SECOND_MILLISECONDS -> ok end,
    %% start receive
    case receive_data(State, 0) of
        Data = <<"GET", _/binary>> ->
            %% Length:18245 Protocol: 21536
            decode_header(State, Data);
        Data = <<"POST", _/binary>> ->
            %% Length:20559 Protocol: 21332
            decode_header(State, Data);
        Data ->
            stream_loop(State, Data)
    end.

%%%===================================================================
%%% tcp
%%%===================================================================
%% stream loop
stream_loop(State, <<Length:16, Protocol:16, Binary:Length/binary, Rest/binary>>) ->
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State, Data) of
                {ok, NewState} ->
                    %% continue
                    stream_loop(NewState, Rest);
                {stop, Reason} ->
                    exit(Reason)
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [Length, Protocol, Binary]),
            %% continue
            stream_loop(State, Rest)
    end;
stream_loop(State, Stream) ->
    Data = receive_data(State, 0),
    stream_loop(State, <<Stream/binary, Data/binary>>).

%%%===================================================================
%%% http Request parse
%%%===================================================================
%% Method[Space]URI[Space]Version[CR,LF]
%% Name:Value[CR,LF]
%% ...
%% Name:Value[CR,LF]
%% [CR,LF]
%% body
decode_header(State, Data) ->
    decode_header(State, Data, [<<>>]).

decode_header(State, <<"\r\n", Rest/binary>>, [Version, URI, Method]) ->
    %% decode header field
    decode_header_field(State, #http{method = Method, uri = URI, version = Version}, Rest, [key, <<>>]);
decode_header(State, <<32, Rest/binary>>, Result) ->
    decode_header(State, Rest, [<<>> | Result]);
decode_header(State, <<Byte, Rest/binary>>, [Segment | Result]) ->
    decode_header(State, Rest, [<<Segment/binary, Byte>> | Result]);
decode_header(State, Stream, Result) ->
    Data = receive_data(State, 0),
    decode_header(State, <<Stream/binary, Data/binary>>, Result).

%% decode http header field
decode_header_field(State, Http, <<32, Rest/binary>>, [key, <<>> | Result]) ->
    %% space
    decode_header_field(State, Http, Rest, [key, <<>> | Result]);
decode_header_field(State, Http, <<32, Rest/binary>>, [value, <<>> | Result]) ->
    %% space
    decode_header_field(State, Http, Rest, [value, <<>> | Result]);

decode_header_field(State, Http, <<":", Rest/binary>>, [key | Result]) ->
    %% key value separator
    decode_header_field(State, Http, Rest, [value, <<>>, key | Result]);

decode_header_field(State, Http, <<"\r\n", Rest/binary>>, [value, <<Value/binary>>, key, <<Key/binary>> | Result]) ->
    %% key value pair
    decode_header_field(State, Http, Rest, [key, <<>>, {Key, Value} | Result]);

decode_header_field(State, Http, <<"\r\n", Rest/binary>>, [key, _ | Result]) ->
    Value = find_header_value(<<"content-length">>, Result, <<"0">>),
    Length = try
        binary_to_integer(Value)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        exit(normal)
    end,
    case Rest of
        <<Body:Length/binary, Remain/binary>> ->
            %% complete packet, with body
            handle_http_request(State, Http#http{fields = Result, body = Body}, Remain);
        _ when Length =< 65536 ->
            %% incomplete packet, continue
            decode_header_field(State, Http, Rest, Result);
        _ ->
            ?PRINT("Http Body Length:~p Out of Limit: ~tp", [Length, Http#http{fields = Result}]),
            %% body size out of limit
            exit(normal)
    end;
decode_header_field(State, Http, <<Byte, Rest/binary>>, [key, Segment | Result]) ->
    decode_header_field(State, Http, Rest, [key, <<Segment/binary, Byte>> | Result]);
decode_header_field(State, Http, <<Byte, Rest/binary>>, [value, Segment | Result]) ->
    decode_header_field(State, Http, Rest, [value, <<Segment/binary, Byte>> | Result]);
decode_header_field(State, Http, Stream, Result) ->
    Data = receive_data(State, 0),
    decode_header_field(State, Http, <<Stream/binary, Data/binary>>, Result).

%% key
find_header_value(_, [], Default) ->
    Default;
find_header_value(Name, [{Key, Value} | T], Default) ->
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Key>> of
        Name ->
            Value;
        _ ->
            find_header_value(Name, T, Default)
    end.

%%%===================================================================
%%% http
%%%===================================================================

%% header
%% fields
%% body
%% treat/upgrade => handshake
%%

handle_http_request(State, Http = #http{fields = Fields}, Data) ->
    Upgrade = find_header_value(<<"upgrade">>, Fields, <<"">>),
    io:format("Upgrade:~p~n", [Upgrade]),
    io:format("Data:~p~n", [Data]),
    case <<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Upgrade>> of
        <<"websocket">> ->
            %% http upgrade (WebSocket)
            handshake(State, Http, Upgrade),
            %% enter web socket stream loop
            web_socket_loop(State, 0, <<>>, 0, <<>>, <<>>);
        _ ->
            %% normal http Request
            master:treat(State, Http)
    end.

%%%===================================================================
%%% http upgrade
%%%===================================================================
%% web socket handshake
handshake(State, #http{version = Version, fields = Fields}, Upgrade) ->
    SecKey = find_header_value(<<"sec-websocket-key">>, Fields, <<"">>),
    Hash = crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>),
    Encode = base64:encode_to_string(Hash),
    Binary = [
        Version, <<" 101 Switching Protocols\r\n">>,
        <<"Upgrade: ">>, Upgrade, <<"\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Encode, <<"\r\n">>,
        <<"\r\n">>
    ],
    sender:send(State, list_to_binary(Binary)).

%%%===================================================================
%%% Web Socket Draft-HyBi-10-17 Exchanging Data Frames
%%%===================================================================

%% Header
%% +---------+--------+------------------------+
%% | FIN     | 1      | End(1)/Continuation(0) |
%% +---------+--------+------------------------+
%% | RSV1    | 1      | Reserve                |
%% +---------+--------+------------------------+
%% | RSV2    | 1      | Reserve                |
%% +---------+--------+------------------------+
%% | RSV3    | 1      | Reserve                |
%% +---------+--------+------------------------+
%% | OpCode  | 4      | OpCode                 |
%% +---------+--------+------------------------+
%% | Mask    | 1      | Mask Flag              |
%% +---------+--------+------------------------+
%% |         | 7      | =< 125                 |
%% | Length  | 7 + 16 | 126                    |
%% |         | 7 + 64 | 127                    |
%% +---------+--------+------------------------+
%% | Masking | 0/32   | Mask(0/1)              |
%% +---------+--------+------------------------+
%% | Payload | Length | xor with Masking       |
%% +---------+--------+------------------------+

%% OpCode
%% +---------+---------------------------------+
%% |    0    |  Continuation Frame             |
%% +---------+---------------------------------+
%% |    1    |  Text Frame                     |
%% +---------+---------------------------------+
%% |    2    |  Binary Frame                   |
%% +---------+---------------------------------+
%% |   3-7   |  Reserve                        |
%% +---------+---------------------------------+
%% |    8    |  Connection Close Frame         |
%% +---------+---------------------------------+
%% |    9    |  Ping Frame                     |
%% +---------+---------------------------------+
%% |   10    |  Pong Frame                     |
%% +---------+---------------------------------+
%% |  11-15  |  Reserve                        |
%% +---------+---------------------------------+

%% web socket stream loop
web_socket_loop(State, Length, Masking, Size, Stream, <<PacketLength:16, Protocol:16, Binary:PacketLength/binary, Rest/binary>>) ->
    case user_router:read(Protocol, Binary) of
        {ok, Data} ->
            %% protocol dispatch
            case account_handler:handle(Protocol, State, Data) of
                {ok, NewState} ->
                    %% continue
                    web_socket_loop(NewState, Length, Masking, Size, Stream, Rest);
                {stop, Reason, _} ->
                    exit(Reason)
            end;
        {error, Protocol, Binary} ->
            ?PRINT("protocol not match: length:~w Protocol:~w Binary:~w ~n", [Length, Protocol, Binary]),
            %% continue
            web_socket_loop(State, Length, Masking, Size, Stream, Rest)
    end;
web_socket_loop(_State, _Length, _Masking, _Size, <<_Fin:1, _Rsv:3, 8:4, Mask:1, BodyLength:7, _:Mask/binary-unit:32, _:BodyLength/binary, _/binary>>, _Packet) ->
    %% close frame
    exit(normal);
web_socket_loop(State, 0, Masking, _Size, <<_Fin:1, _Rsv:3, _Opcode:4, Mask:1, 127:7, Length:64, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    case Rest of
        <<Body:Length/binary, RestStream/binary>> ->
            %% complete packet
            {Data, NewSize} = unmask(Body, Masking, 0, <<>>),
            web_socket_loop(State, 0, Masking, NewSize, RestStream, <<Packet/binary, Data/binary>>);
        _ ->
            %% incomplete packet
            {Data, NewSize} = unmask(Rest, Masking, 0, <<>>),
            web_socket_loop(State, Length - byte_size(Rest), Masking, NewSize, <<>>, <<Packet/binary, Data/binary>>)
    end;
web_socket_loop(State, 0, Masking, _Size, <<_Fin:1, _Rsv:3, _Opcode:4, Mask:1, 126:7, Length:16, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    case Rest of
        <<Body:Length/binary, RestStream/binary>> ->
            %% complete packet
            {Data, NewSize} = unmask(Body, Masking, 0, <<>>),
            web_socket_loop(State, 0, Masking, NewSize, RestStream, <<Packet/binary, Data/binary>>);
        _ ->
            %% incomplete packet
            {Data, NewSize} = unmask(Rest, Masking, 0, <<>>),
            web_socket_loop(State, Length - byte_size(Rest), Masking, NewSize, <<>>, <<Packet/binary, Data/binary>>)
    end;
web_socket_loop(State, 0, Masking, _Size, <<_Fin:1, _Rsv:3, _Opcode:4, Mask:1, Length:7, Masking:Mask/binary-unit:32, Rest/binary>>, Packet) ->
    case Rest of
        <<Body:Length/binary, RestStream/binary>> ->
            %% complete packet
            {Data, NewSize} = unmask(Body, Masking, 0, <<>>),
            web_socket_loop(State, 0, Masking, NewSize, RestStream, <<Packet/binary, Data/binary>>);
        _ ->
            %% incomplete packet
            {Data, NewSize} = unmask(Rest, Masking, 0, <<>>),
            web_socket_loop(State, Length - byte_size(Rest), Masking, NewSize, <<>>, <<Packet/binary, Data/binary>>)
    end;
web_socket_loop(State, 0, Masking, Size, Stream, Packet) ->
    Data = receive_data(State, 0),
    web_socket_loop(State, 0, Masking, Size, <<Stream/binary, Data/binary>>, Packet);
web_socket_loop(State, Length, Masking, Size, Stream, Packet) ->
    case Stream of
        <<Body:Length/binary, RestStream/binary>> ->
            %% complete packet
            {Data, NewSize} = unmask(Body, Masking, Size, <<>>),
            web_socket_loop(State, 0, Masking, NewSize, RestStream, <<Packet/binary, Data/binary>>);
        _ ->
            %% incomplete packet
            {Data, NewSize} = unmask(Stream, Masking, Size, <<>>),
            web_socket_loop(State, Length - byte_size(Stream), Masking, NewSize, <<>>, <<Packet/binary, Data/binary>>)
    end.

%%%===================================================================
%%% web socket frame decode
%%%===================================================================
%% unmask
unmask(<<Payload:32, Rest/binary>>, Masking = <<Mask:32>>, 0, Acc) ->
    unmask(Rest, Masking, 0, <<Acc/binary, (Payload bxor Mask):32>>);
unmask(<<Payload:24>>, <<Mask:24, _Rest/binary>>, 0, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):24>>, 1};
unmask(<<Payload:16>>, <<Mask:16, _Rest/binary>>, 0, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):16>>, 2};
unmask(<<Payload:8>>, <<Mask:8, _Rest/binary>>, 0, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):8>>, 3};
%% offset 1
unmask(<<Payload:24, Rest/binary>>, Masking = <<_Offset:1/binary, Mask:24>>, 1, Acc) ->
    unmask(Rest, Masking, 0, <<Acc/binary, (Payload bxor Mask):8>>);
unmask(<<Payload:16>>, <<_Offset:2/binary, Mask:24>>, 1, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):16>>, 0};
unmask(<<Payload:8>>, <<_Offset:3/binary, Mask:24>>, 1, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):16>>, 0};
%% offset 2
unmask(<<Payload:16, Rest/binary>>, Masking = <<_Offset:2/binary, Mask:16>>, 2, Acc) ->
    unmask(Rest, Masking, 0, <<Acc/binary, (Payload bxor Mask):16>>);
unmask(<<Payload:8>>, <<_Offset:3/binary, Mask:16>>, 2, Acc) ->
    {<<Acc/binary, (Payload bxor Mask):16>>, 0};
%% offset 3
unmask(<<Payload:8, Rest/binary>>, Masking = <<_Offset:3/binary, Mask:8>>, 3, Acc) ->
    unmask(Rest, Masking, 0, <<Acc/binary, (Payload bxor Mask):24>>);
%% end
unmask(<<>>, _, Offset, Acc) ->
    {Acc, Offset}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% receive data
receive_data(#client{socket_type = gen_tcp, socket = Socket}, Length) ->
    case gen_tcp:recv(Socket, Length, ?MINUTE_MILLISECONDS) of
        {ok, Data} ->
            Data;
        {error, closed} ->
            exit(normal);
        {error, timeout} ->
            exit(normal);
        {error, Reason} ->
            exit(Reason)
    end;
receive_data(#client{socket_type = ssl, socket = Socket}, Length) ->
    case ssl:recv(Socket, Length, ?MINUTE_MILLISECONDS) of
        {ok, Data} ->
            Data;
        {error, closed} ->
            exit(normal);
        {error, timeout} ->
            exit(normal);
        {error, Reason} ->
            exit(Reason)
    end.
