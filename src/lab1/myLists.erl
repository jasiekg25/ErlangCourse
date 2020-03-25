%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2020 04:49
%%%-------------------------------------------------------------------
-module(myLists).
-author("jasiek").

%% API
-export([ contains/2, duplicateElements/1, sumFloats/2, sumFloats/1]).

contains(_, []) ->
  false;
contains (Value, [H | _ ]) when Value == H ->
  true;
contains(Value, [ _ | T ]) -> myLists:contains(Value, T).


duplicateElements([]) ->
  [];
duplicateElements([H | T]) ->
  [H, H] ++ myLists:duplicateElements(T).


sumFloats(List) ->
  sumFloats(List, 0).
sumFloats ([], Sum) ->
  Sum;
sumFloats([H | T], Sum) when is_float(H) ->
  sumFloats(T, Sum + H);
sumFloats([H | T], Sum) ->
  sumFloats(T, Sum).


