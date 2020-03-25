%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2020 04:47
%%%-------------------------------------------------------------------
-module(onp).
-author("jasiek").

%% API
-export([convert/1, rpn/1, rpn/2]).

convert(StringToFloat) ->
  case string:to_float(StringToFloat) of
    {error, no_float} -> list_to_integer(StringToFloat)
  end.

rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.

rpn("+", [N1, N2 | S]) -> [N2 + N1 | S];
rpn("-", [N1, N2 | S]) -> [N2 - N1 | S];
rpn("*", [N1, N2 | S]) -> [N2 * N1 | S];
rpn("/", [N1, N2 | S]) -> [N2 / N1 | S];
rpn("^", [N1, N2 | S]) -> [math:pow(N2, N1) | S];
rpn("sin", [N | S]) -> [math:sin(N) | S];
rpn("cos", [N | S]) -> [math:cos(N) | S];
%% additional operators
rpn("modulo_5", [N | S]) -> [N rem 5 | S];
rpn("modulo", [N1, N2 | S]) -> [N1 rem N2 | S];
rpn(X, Stack) -> [convert(X) | Stack].


