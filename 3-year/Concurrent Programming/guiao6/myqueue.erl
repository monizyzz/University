-module(myqueue).
-export([create/0, enqueue/2, dequeue/1]).

-type myqueue() :: {list(), list()}.


-spec create() -> myqueue().
create() ->
    {[], []}.


-spec enqueue(myqueue(), term()) -> myqueue().
enqueue(Queue, Element) -> 
    Queue ++ [Element].

-spec dequeue(myqueue()) -> empty | {myqueue(), term()}.
dequeue([]) ->
    empty;
dequeue([H | T]) -> 
    {T, H}.


