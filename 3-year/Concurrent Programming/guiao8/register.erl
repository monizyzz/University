-module(register).
- export([create/0, add/2, avg/1]).

create() ->
    spawn(fun() -> loop(0,0) end).

add(Pid, N) ->
    Pid ! {add, N}. % enviar mensagem para o processo

avg(Pid) ->
    Pid ! {avg, self()},
    receive
        Avg -> Avg
    end.


loop(Sum, Count) ->
    receive
        {add, N} ->
            loop(Sum + N, Count + 1);
        {avg, Pid} ->
            Avg = if
                Count == 0 -> 0;
                true -> Sum / Count
            end,
            Pid ! Avg,
            loop(Sum, Count)
    end.