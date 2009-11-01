-module(server).
-export([start/0, loop/1]).

-include("config.hrl").

%% Start the server
start() ->
    io:format("Server started~n"),
    crypto:start(),
    socket_server:start(?MODULE, ?SERVER_PORT, {?MODULE, loop}).

%% Loop to handle data
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %%io:format("Got Data: ~p~n", [Data]),
            %%io:format("Got Term: ~p~n", [binary_to_term(Data)]),
            Result = handle_data(binary_to_term(Data)),
            %%io:format("Sending back: ~p~n", [term_to_binary(Result)]),
            gen_tcp:send(Socket, term_to_binary(Result)),
            loop(Socket);
        {error, closed} ->
            ok
    end.

handle_data({?REQ_HASH_LIST, Filename}) ->
    utils:hash_list(Filename);
handle_data(_) ->
    "Unknown API Call".
