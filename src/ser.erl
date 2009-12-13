-module(ser).
-export([server/0]).

server() ->
    {ok, LSock} = gen_tcp:listen(9630, [binary, {packet, 2}, 
                                        {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    Bin = do_recv(Sock, []),
    gen_tcp:send(Sock, term_to_binary({result_tuple, "result_string"})),
    ok = gen_tcp:close(Sock),
    Bin.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            io:format("ok~n"),
            case B of 
                <<"END">> ->
                    io:format("end~n"),
                    Bs;
                _ ->
                    io:format("do_recv~n"),
                    do_recv(Sock, [Bs, B])
            end;
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.


