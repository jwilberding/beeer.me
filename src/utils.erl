 -module(utils).
-export([hash_file/1, hash_list/1, compare_hashes/2, file_size/1, chunk_sizes/1]).

%% Lookup chunk sizes via file size, returns {TotalChunks, ChunkSize, LastChunkSize}
chunk_sizes(FileSize) ->
    if
        %% =< 1MB (100KB chunks)
        FileSize > 0, FileSize =< 1024000 ->
            {FileSize div 102400, 102400, FileSize rem 102400};
        %% =< 10MB (100KB chunks)
        FileSize > 1024000, FileSize =< 10240000 ->
            {FileSize div 102400, 102400, FileSize rem 102400};
        %% =< 100MB (10MB chunks)
        FileSize > 10240000, FileSize =< 102400000 ->
            {FileSize div 10240000, 10240000, FileSize rem 10240000};
        %% =< 1000MB (10MB chunks)
        FileSize > 102400000, FileSize =< 1024000000 ->
            {FileSize div 10240000, 10240000, FileSize rem 10240000};
        %% > 1000MB (10MB chunks)
        true ->
            {FileSize div 10240000, 10240000, FileSize rem 10240000}
    end.

%% Hash file
hash_file(Filename) ->
    Size = file_size(Filename),
    case file:open( Filename, [read]) of
        {ok,File} ->
            {ok,[Chunk1]} = file:pread( File, [{0,Size}]),
            Hash = crypto:md5(Chunk1),
            file:close(File),
            {ok, Hash};
        _ ->
            {error, nofile}
  end.

%% Generate a hash list of file. Size of chunks is file_size/10
%% Add reverse hashes as well
hash_list(Filename) ->
    %% Get file size in bytes
    Size = file_size(Filename),
    
    %% Compute chunk size
    {TotalChunks, ChunkSize, LastChunkSize} = chunk_sizes(Size),
    %ChunkSize = Size div 10,
    
    %% Last chunk size
    %LastChunkSize = ChunkSize + (Size - (ChunkSize*10)),
    io:format("TotalChunks: ~p, ChunkSize: ~p, LastChunkSize: ~p~n", [TotalChunks, ChunkSize, LastChunkSize]),
    
    %% Generate hashes for each chunk
    hash_file(Filename, TotalChunks, ChunkSize, LastChunkSize).

%% Hash file
hash_file(Filename, TotalChunks, ChunkSize, LastChunkSize) ->
    case file:open( Filename, [read]) of
        {ok,File} ->
            HashList = lists:map(fun(X) ->
                              {ok,[Chunk]} = file:pread( File, [{X*ChunkSize,ChunkSize}]),
                              {X,crypto:md5(Chunk)} end,
                      lists:seq(0,TotalChunks-2)),
            {ok,[LastChunk]} = file:pread( File, [{(TotalChunks-1)*ChunkSize,LastChunkSize}]),
            LastHash = {TotalChunks-1, crypto:md5(LastChunk)},
            file:close(File),
            
            %% Put last hash in front, just because it is most efficient for list operations
            [LastHash | HashList];
        _ ->
            {error, nofile}
  end.

%% Compare hashes from two lists to determine update list
%% Use forward hashes, reverse hashes, and subhashes to determine exact parts of file that changed
compare_hashes(List1, List2) ->
    io:format("Chunks that need updated:~n"),
    List1 -- List2.

%% Get file size in bytes
file_size(Filename) ->
    case file:read_link_info(Filename) of 
        {ok, {_,Size,_,_,_,_,_,_,_,_,_,_,_,_}} ->
            Size;
        _ ->
            io:format("Could not read file size: ~s~n", [Filename])
    end.

start(Dir) -> 
  crypto:start(),
  io:format("Generating File List~n"),
  FileListing = generate_file_listing(Dir),
  io:format("Finding Dupes~n"),
  find_dupes(FileListing),
  crypto:stop().

% generates file listing recursively with size of each file        
generate_file_listing(Dir) ->
  case file:list_dir(Dir) of
    {ok, FileListing} -> add_file_size(Dir, FileListing,[]);
    _ -> io:format("Unable to read directory: ~s~n", [Dir]), []
  end.

% add file size, and recurse if directory
add_file_size(_, [], FileListingWithType) -> FileListingWithType;
add_file_size(Dir, [H | T], FileListingWithType) ->
  case file:read_link_info(Dir ++ "/" ++ H) of 
    {ok, {_,Size,FileType,_,_,_,_,_,_,_,_,_,_,_}} ->
      case FileType of
        regular -> add_file_size(Dir, T, [{Dir ++ "/" ++ H, Size} | FileListingWithType]);
        directory -> add_file_size(Dir, T, generate_file_listing(Dir ++ "/" ++ H) ++ FileListingWithType);
        _ -> add_file_size(Dir, T, FileListingWithType)
      end;
    _ ->
      io:format("Could not read file: ~s", [Dir ++ "/" ++ H]),
      add_file_size(Dir, T, FileListingWithType)
  end.

% find dupes by first sorted by size, then checking hashes of files with same size
find_dupes(TupleList) -> find_dupes1(lists:sort(fun({_,L}, {_,R}) -> L =< R end, TupleList)).
find_dupes1([]) -> ok;
find_dupes1([_ | []]) -> ok;
find_dupes1(List) ->
  [_ | [{Filename2, Size2} | T]] = List,
  {CheckList, List2} = split_list(List),
  case length(CheckList) > 1 of
    false -> find_dupes([{Filename2, Size2} | T]);
    true ->
      HashList = add_hashes(CheckList),
      find_dupe_hash(HashList),
      find_dupes1(List2)
  end.

% find if files have same hash, if so, they are a duplicate
find_dupe_hash(TupleList) -> find_dupe_hash(lists:sort(fun({_,_,L}, {_,_,R}) -> L =< R end, TupleList), -1).
find_dupe_hash([], _) -> ok;
find_dupe_hash([_ | []], _) -> ok;
find_dupe_hash([{Filename, Size, Hash} | [{Filename2, Size2, Hash2} | T]], LastHash) ->
  case Hash == Hash2 of
    false -> ok;
    true ->
      if  
        Hash =/= LastHash -> io:format(":: size=~w~n~s~n~s~n", [Size, Filename, Filename2]);
	true -> io:format("~s~n", [Filename2])
      end
  end,
  find_dupe_hash([{Filename2, Size2, Hash2} | T], Hash).

% split the list where the file size changes
split_list(List) -> {_,Size} = hd(List), split_list(List, [], Size).
split_list([], List1, _) -> {List1, []};
split_list([{Filename, Size} | T], List1, LastSize) ->
  if 
    Size == LastSize -> split_list(T, [{Filename, Size} | List1], Size);
    true -> {List1, [{Filename, Size} | T]}
  end.

% get hashes of a file list
add_hashes(List) -> add_hashes(List, []).
add_hashes([], NewList) -> NewList;
add_hashes([{Filename, Size} | T], NewList) ->
  case get_hash(Filename, Size) of
    {ok, Hash} -> add_hashes(T, [{Filename, Size, Hash} | NewList]);
    {error, _} -> add_hashes(T, NewList)
  end.

% gets hash of a file, if file > 512bytes, only get first and last 256 bytes
% in order to increase speed, accuracy is minimally affected
get_hash(_, 0) -> {ok,crypto:md5([])};
get_hash(Filename, Size) ->
  case file:open( Filename, [read]) of
    {ok,File} ->
      if 
        Size < 512 -> {ok,[Chunk1]} = file:pread( File, [{0,Size}]), Chunk2=[];
        true -> {ok,[Chunk1,Chunk2]} = file:pread( File, [{0,256},{Size-256,256}])
      end,
      Hash = crypto:md5(lists:append(Chunk1,check_eof(Chunk2))),
      file:close(File),
      {ok, Hash};
    _ -> {error, nofile}
  end.

% if filesize changes between time we got listing and when we goto get a hash
% the pread in get_hash will get an eof which is bad for list:append
check_eof(F) ->
  if 
    F == eof -> [];
    true -> F
  end.

%% just a debug function to print the file listing  
%print_file_listing([]) -> ok;
%print_file_listing([{FileName, Size} | T]) ->
%  io:format("~s::~w~n", [FileName, Size]),
%  print_file_listing(T).

