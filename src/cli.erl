-module(cli).

-export([client/0, client2/0, client3/0]).

client() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 9630, 
                                 [binary, {packet, 2}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).

client2() ->
    SomeHostInNet = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 9630, 
                                 [binary, {packet, 2}]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).

client3() ->
    {ok, Socket} = gen_tcp:connect("localhost", 9630, [binary,{packet, 2}]),
    ok = gen_tcp:send(Socket, term_to_binary({blah_atom, "test.dat"})),
    ok = gen_tcp:send(Socket, <<"END">>),
    Bin = wait_reply(),
    ok = gen_tcp:close(Socket),
    Bin. 

wait_reply() ->
    receive
        Reply ->
            {tcp, _Port, Data} = Reply,
            Data
    after 100000 ->
            timeout
    end.
