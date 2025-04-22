-module(login_manager).
-export([start/0, create_account/2, close_account/1, login/2, online/0]).

% rpc
rpc(Request) ->
    ?MODULE ! {self(), Request},
    receive
        Response -> Response
    end.

% client
create_account(User, Pass) ->
    rpc({create_account, User, Pass}).

close_account(User) ->
    rpc({close_account, User}).

login(User, Pass) ->
    rpc({login, User, Pass}).

online() ->
    rpc(online).

% server
start() ->
    Map = #{},
    Pid = spawn(fun() -> loop(Map) end),
    register(?MODULE, Pid).

% TODO: colocar status no valor do mapa {Pass, Status = true | false}
loop(Map) ->
    receive
        {Pid, {create_account, U, P}} ->
            case maps:find(U, Map) of
                {ok, _} ->
                    Pid ! user_exists,
                    loop(Map);
                error ->
                    NewMap = maps:put(U, {P, false}, Map),
                    Pid ! ok,
                    loop(NewMap)
            end;
        {Pid, {close_account, U}} ->
            case maps:find(U, Map) of
                {ok, _} ->
                    NewMap = maps:remove(U, Map),
                    Pid ! ok,
                    loop(NewMap);
                error ->
                    Pid ! user_not_found,
                    loop(Map)
            end;
        {Pid, {login, U, P}} ->
            case maps:find(U, Map) of
                {ok, {P, false}} ->
                    NewMap = maps:put(U, {P, true}, Map),
                    Pid ! ok,
                    loop(NewMap);
                _ ->
                    Pid ! invalid,
                    loop(Map)
            end;
        {Pid, online} ->
            Pid ! [U || {U, {_, true}} <- maps:to_list(Map)],
            loop(Map)
    end.
