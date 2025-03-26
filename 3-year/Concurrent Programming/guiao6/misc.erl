-module(misc).
-export([add/2,even/1,factorial/1,factorial_alt/1,area/1,sum/1,seq/2,concat_lists/2,append_to_list/2,even_list/1,even_list_2/1,even_list_3/1,filter/2]). 

add(N1, N2) -> 
    N1 + N2.



% rem - resto 
-spec even(number()) -> boolean().
even(N) ->
    N rem 2 == 0.




factorial(1) -> 1;
factorial(N) ->
    N * factorial(N-1).

% tail recursive

factorial_alt(N) -> 
    factorial_alt(N, 1);

factorial_alt(1, Acc) -> 
    Acc;

factorial_alt(N, Acc) ->
    factorial_alt(N - 1, N * Acc).


%   > erl -make
%   > erl
%   > misc:add(2, 4). --- não funciona
%   > c(misc).


area({rectangle, Width, Height}) -> 
    Width * Height;

area({circle, Radius}) ->
    math:pi() * math.pow(Radius, 2).

%   > misc:area({rectangle, 10, 20}). -> 200


sum([]) -> 
    0;
sum(H | T) -> 
    H + sum(T).

%   > misc:sum([1, 2, 3, 4, 5]). -> 15


seq(A, A) -> 
    [A];
seq(A, B) ->
    [A | seq(A + 1, B)].


%   > misc:seq(1, 10). -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

concat_lists(L1, L2) -> 
    L1 ++ L2.

%   > misc:concat_lists([1, 2, 3], [4, 5, 6]). -> [1, 2, 3, 4, 5, 6]

append_to_list(L, Item) -> 
    L ++ [Item].

%  > misc:append_to_list([1, 2, 3], 4). -> [1, 2, 3, 4]

even_list([]) -> 
    [];
even_list([H | T]) ->
    if
        H rem 2 == 0 -> [H | even_list(T)];
        true -> even_list(T)
    end.

%   > misc:even_list([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]). -> [2, 4, 6, 8, 10]

even_list_2([H | T]) when H rem 2 == 0 -> 
    [H | even_list_2(T)];
even_list_2([_ | T]) -> 
    even_list_2(T).

even_list_3([]) -> 
    [];
even_list_3([H | T]) ->
    case even(H) of
        true -> [H | even_list_3(T)];
        false -> even_list_3(T)
    end.


filter(Func, [H | T]) ->
    case Func(H) of
        true -> [H | filter(Func, T)];
        false -> filter(Func, T)
    end.


% lists:map(fun(X) -> X * 2 end, [1, 2, 3, 4, 5]).
% lists:filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4, 5]).


%[X || X <- [1, 2, 3, 4], X rem 2 == 0].
