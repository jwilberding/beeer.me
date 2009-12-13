-module(server).
-export([start/0, loop/1]).

-include("config.hrl").

%% Start the server
start() ->
    io:format("Server started~n"),
    crypto:start(),
    socket_server:start(?MODULE, ?SERVER_PORT, {?MODULE, loop}).

%% Loop to handle data
loop(Socket) -> loop(Socket, <<>>).
loop(Socket, DataAcc) ->
    io:format("Going to wait~n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("loop~n"),
            io:format("Size data: ~p~n", [size(Data)]),
            loop(Socket, Data);
        {error, closed} ->
            %%io:format("Got Data: ~p~n", [Data]),
            %%io:format("Got Term: ~p~n", [binary_to_term(Data)]),
            io:format("Got data~n"),
            %io:format("Woo: ~p: ~p~n", [Data, size(Data)]),
            io:format("Woo: ~p~n", [size(DataAcc)]),
            Term = binary_to_term(DataAcc),
            io:format("Converted~n"),
            Result = handle_data(Term),
            io:format("Handled~n"),
            %%io:format("Sending back: ~p~n", [term_to_binary(Result)]),
            gen_tcp:send(Socket, term_to_binary(Result)),
            io:format("Sent back~n"),
            ok;
        Shit ->
            io:format("Fuck~n"),
            io:format("Shit: ~p~n", [Shit])
    end.

handle_data({?REQ_HASH_LIST, Filename}) ->
    utils:hash_list(Filename);
handle_data({?NEW_FILE, Filename, Data}) ->
    io:format("Writing file~n"),
    Res = utils:write_file(Filename, Data),
    io:format("Got Res~n"),
    Res;
handle_data(_) ->
    "Unknown API Call".
