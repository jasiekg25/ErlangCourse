%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2020 04:51
%%%-------------------------------------------------------------------
-module(funModule).
-author("jasiek").

%% API
-export([]).
%%map(_, [])
%%  -> [];
%%map(F, [H | T])
%%  -> [F(H) | map(F, T)].
%%
%%filter(Pred, L) ->
%%  lists:reverse(filter(Pred, L, [])).
%%filter(_, [], Acc) ->
%%  Acc;
%%filter(Pred, [H | T], Acc) ->
%%  case Pred(H) of
%%    true -> filter(Pred, T, [H | Acc]);
%%    false -> filter(Pred, T, Acc)
%%  end.
%%
%%sum(A, B) ->
%%  A + B.
%%
%%sumOfDigits(Number) ->
%%  Stringed = integer_to_list(Number),
%%  List = [list_to_integer([Char]) || Char <- Stringed],
%%  lists:foldl(funModule:sum/2, 0,List ).
