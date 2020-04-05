%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2020 13:40
%%%-------------------------------------------------------------------
-module(pingpong).
-author("jasiek").

%% API
-export([start/0, stop/0, play/1]).
start() ->
  Ping = spawn(fun() -> ping(0) end),
  Pong = spawn(fun() -> pong(0) end),
  register(ping, Ping),
  register(pong, Pong).

stop() ->
  ping ! {self(),terminate},
  pong ! {self(),terminate}.

play(N) ->
  ping ! {pong, N}.


ping(TotalValue) ->
  receive
    {Pid, terminate} ->
      io:format("Process ping terminated ~n");
    {_, printTotalValue} ->
      io:fwrite("Ping's TotalValue =  ~p ~n ", [TotalValue]),
      ping(TotalValue);
    {Pid, 0} ->
      io:fwrite("Ping receive      0, no more fun... ~n"),
      ping ! {ping, printTotalValue},
      pong ! {ping, printTotalValue},
      ping(TotalValue);
    {Pid, N} ->
      timer:sleep(500),
      io:format("Ping receive:     ~p ~n ", [N]),
      Pid ! {ping, N - 1},
      ping(TotalValue + N)
  after 20000 ->
    io:format("Ping's timeout ~n"),
    {ping,timeout}
  end.


pong(TotalValue) ->
  receive
    {Pid, terminate} ->
      io:format("Process pong terminated ~n");
    {_, printTotalValue} ->
      io:format("Pong's TotalValue =  ~p ~n", [TotalValue]),
      pong(TotalValue);
    {Pid, 0} ->
      io:format("Pong receive    0, no more fun... ~n"),
      pong ! {pong, printTotalValue},
      ping ! {pong, printTotalValue},
      pong(TotalValue);
    {Pid, N} ->
      timer:sleep(500),
      io:format("Pong receive: ~p ~n ", [N]),
      Pid ! {pong, (N - 1)},
      pong(TotalValue + N)
  after 20000 ->
    io:format("Pong's timeout ~n"),
    {pong, timeout}
  end.
