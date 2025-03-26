-module(test).
-export ([test_myqueue/0]).


test_myqueue() ->
    Q1 = myqueue:create(),
    Q2 = myqueue:enqueue(Q1, 1),
    Q3 = myqueue:enqueue(Q2, 2),
    {Q4, I1} = myqueue:dequeue(Q3),
    erlang:display({Q4, I1}).

% test:test_myqueue().