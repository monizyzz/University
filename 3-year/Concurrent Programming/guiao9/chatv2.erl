-module(chatv2).
-export([start/1, stop/1]).

start(Port) -> spawn(fun() -> server(Port) end).

stop(Server) -> Server ! stop.

server(Port) ->
  {ok, LSock} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
  Room = spawn(fun()-> room([]) end),
  login_manager:start(),
  spawn(fun() -> acceptor(LSock, Room) end),
  receive stop -> ok end.

acceptor(LSock, Room) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock, Room) end),
  user_logged_out(Sock, Room).

room(Pids) ->
  receive
    {enter, Pid} ->
      io:format("user entered~n", []),
      room([Pid | Pids]);
    {line, Data} = Msg ->
      io:format("received ~p~n", [Data]),
      [Pid ! Msg || Pid <- Pids],
      room(Pids);
    {leave, Pid} ->
      io:format("user left~n", []),
      room(Pids -- [Pid])
  end.

user_logged_out(Sock, Room) ->
  receive
    {tcp, _, Data} ->
      case string:tokens(Data) of
        ["/c", User, Pass] ->
          Response = login_manager:create_account(User, Pass),
          gen_tcp:send(Sock, io_lib:format("~p\n", [Response])),
          user_logged_out(Sock, Room);
        ["/l", User, Pass] -> 
          case login_manager:login(User, Pass) of
            ok -> 
              gen_tcp:send(Sock, "logged in\n"),
              Room ! {enter, self()},
              user_logged_in(Sock, Room, User);
            _ -> 
              gen_tcp:send(Sock, "invalid\n"),
              user_logged_out(Sock, Room)
          end;
        _ -> 
          gen_tcp:send(Sock, "invalid message\n"),
          user_logged_out(Sock, Room)
      end
  end.

user_logged_in(Sock, Room, User) ->
  receive
    {line, Data} ->
      gen_tcp:send(Sock, Data),
      user_logged_in(Sock, Room, User);
    {tcp, _, Data} ->
      Room ! {line, User ++ ": " ++ Data},
      user_logged_in(Sock, Room, User);
    {tcp_closed, _} ->
      Room ! {leave, self()};
    {tcp_error, _, _} ->
      Room ! {leave, self()}
  end.





