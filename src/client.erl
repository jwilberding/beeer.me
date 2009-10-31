-module(client).
-export([init/0, watch_dir/1, unwatch_dir/1, check_updates/0]).

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
