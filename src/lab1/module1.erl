%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2020 04:48
%%%-------------------------------------------------------------------
-module(module1).
-author("jasiek").

%% API
-export([volume/2, factorial/1, power/2, factorial/2]).


volume(cube, Edge) ->
  Edge * Edge * Edge;
volume(cuboid, {Edge1, Edge2, Edge3}) ->
  Edge1 * Edge2 * Edge3;
volume(_,_) ->
  io:format("Error in volume/2!"), {error, cannot_calculate}.


factorial(N) ->
  factorial(N, 1).

factorial(1, Acc) ->
  Acc;
factorial(N, Acc) ->
  factorial(N-1, Acc * N).

%%MinMax = fun
%%  (X, {Min, Max}) when X < Min -> {X, Max};
%%  (X, {Min, Max}) when X > Max -> {Min, X};
%%  (_, V) -> V end.

-spec power(atom(), integer()) -> integer().
power (Number, 0) -> 1;
power (Number, N) -> Number * power(Number, N-1).

