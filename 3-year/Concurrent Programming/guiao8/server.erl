-module(server).
-export([start/1]).

start(Port) ->
    {ok, LSock} = gen_tcp:start_listen(Port, [
        {packet, line}, {reuseaddr, true}, {exit_on_close, false}
    ]),
    Register = register:create(),
    spawn(fun() -> acceptor(LSock, Register) end).


acceptor(LSock, Register) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Register) end),
    worker(Sock, 0, Register).

worker(Sock, Sum, Register) ->
    receive
        {tcp, Sock, Data} -> 
            {Num, _} = string:to_integer(Data),
            register:add(Register, Num),
            NewSum = Num + Sum,
            gen_tcp:send(Sock, io_lib:format("~p~n", [NewSum])),
            worker(Sock, NewSum, Register);
        {tcp_closed, Sock} -> 
            Avg = register:avg(Register),
            gen_tcp:send(Sock, io_lib:format("Average: ~p~n", [Avg]))
    end.

