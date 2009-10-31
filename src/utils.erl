-module(utils).
-export([hash_file/1, hash_list/1, compare_hashes/2]).

%% Hash file
hash_file(File) ->
    "abc".

%% Generate a hash list of file. Size of chunks is file_size/10
hash_list(File) ->
    %% Get file size in bytes

    %% Compute chunk size

    %% Last chunk size

    %% Generate hashes for each chunk
    ["asd","fgh"].

%% Compare hashes from two lists to determine update list
compare_hashes(List1, List2) ->
    [1,4,5].
