-module(teste).

start() ->
    Pid = spawn(fun() -> matchMaker() end),
    register(match, Pid).


waitForConsumer() ->
    match ! {producer, self()},
    receive Msg -> Msg end.

waitForProducer() ->
    match ! {consumer, self()},
    receive Msg -> Msg end.

matchMaker() ->
    receive
        {producer, Pid} ->
            waitFor(consumer, Pid),
            matchMaker();
        

        {consumer, Pid} ->
            waitFor(producer, Pid),
            matchMaker();
    end.


waitFor(WaitType, MyPid) ->
    receive
        {WaitType, OtherPid} ->
            Buffer = spawn(fun() -> buffer() end),
            MyPid ! Buffer,
            OtherPid ! Buffer,
    end.