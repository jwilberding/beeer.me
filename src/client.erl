-module(client).
-export([init/0, watch_dir/1, unwatch_dir/1, check_updates/0, request_hash_list/1]).

-include("config.hrl").

%% Init loop that checks watched directory an checks for updates on server
init() ->
    io:format("Client started~n").

%% Add directory to watched list
watch_dir(Dir) ->
    io:format("Watching: ~s~n", [Dir]).

%% Remove directory to watched list
unwatch_dir(Dir) ->
    io:format("UnWatching: ~s~n", [Dir]).

%% Check for updated files on server
check_updates() ->
    io:format("Checking for updated files on server~n").

%% Request hash list of file from server, empty list means file does not exist yet
%% TODO: Request via hash of file, not by filename
request_hash_list(Filename) ->
    case gen_tcp:connect(?HOST, ?SERVER_PORT, [binary,{packet, 2}]) of
        {ok, Socket} ->
            io:format("Socket=~p~n",[Socket]),
            io:format("Sending: ~p~n", [term_to_binary({?REQ_HASH_LIST, Filename})]),
            gen_tcp:send(Socket, term_to_binary({?REQ_HASH_LIST, Filename})),
            Reply = binary_to_term(wait_reply()),
            case Reply of
                {error, nofile} ->
                    HashList = "File does not exist remotely";
                HashList ->
                    HashList
            end,
            gen_tcp:close(Socket),
            HashList;
        Error ->
            {error, Error}
    end.

%% Sends a new file, we will do this when file does not exist on server
%% TODO: Filename on server should be filename+hash to keep unique
send_file(Filename) ->
    case gen_tcp:connect(?HOST, ?SERVER_PORT, [binary,{packet, 2}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, term_to_binary({?NEW_FILE, Filename, readdata})),
            Reply = binary_to_term(wait_reply()),
            case Reply of
                {error, nowrite} ->
                    HashList = "Error: Could not write file";
                ok ->
                    ok
            end,
            gen_tcp:close(Socket),
            HashList;
        Error ->
            {error, Error}
    end.

wait_reply() ->
    receive
        Reply ->
            {tcp, _Port, Data} = Reply,
            Data
    after 100000 ->
            timeout
    end.
