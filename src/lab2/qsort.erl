%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2020 04:55
%%%-------------------------------------------------------------------
-module(qsort).
-author("jasiek").

%% API
-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).
%%
lessThan(List, Arg) ->
  [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) ->
  [];
qs([Pivot | Tail]) ->
  qsort:qs(qsort:lessThan(Tail, Pivot)) ++ [Pivot] ++ qsort:qs(qsort:grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) ->
  [Min + random:uniform(Max - Min) || _ <- lists:seq(1, N)].


compareSpeeds(List, Fun1, Fun2) ->
%%  Tuple_in_string1 = lists:flatten( io_lib:format("~p", [timer:tc(Fun1, [List])]) ),
%%  Tuple_in_string2 = lists:flatten(io_lib:format("~p", [timer:tc(Fun2, [List])])),
  Fun1st = [ timer:tc(Fun1, [List]), timer:tc(Fun2, [List]) ],
  [F1, F2 | T] = [ X || {X, _} <- Fun1st ],
  io:format("First Function's Time:  \"~p\" ~n ", [F1]),
  io:format("Second Function's Time:  \"~p\" ~n ", [F2]).
%%  io:format("\"~p\"~n", [timer:tc(Fun1, [List])] ),
%%  io:format("\"~p\"~n", [timer:tc(Fun1, [List])] ).
