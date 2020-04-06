%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2020 21:24
%%%-------------------------------------------------------------------
-module(parallelCalculations).
-author("jasiek").

%% API
-export([prepareVariables/2, sequentialVersion/2, parallelVersion/2, semiParallelVersion/2, test/2]).

prepareVariables(ClientsNumber, LockersNumber) ->
  LockerLocationsList = [{X, Y} || X <- qsort:randomElems(LockersNumber, 0, 10000), Y <- qsort:randomElems(1, 0, 10000)],
  ClientLocationsList = [{X, Y} || X <- qsort:randomElems(ClientsNumber, 0, 10000), Y <- qsort:randomElems(1, 0, 10000)],
  {ClientLocationsList, LockerLocationsList}.


findDistancePowered2({X1, Y1}, {X2, Y2}) ->
  math:pow( abs(X2 - X1), 2 ) + math:pow( abs(Y2 - Y1), 2 ).

findMyParcelLocker(PersonLocation, LockerLocations) ->
  findMyParcelLocker(PersonLocation, LockerLocations, {}).

findMyParcelLocker( _ , [], {}) ->
  {error, emptyLockerLocationsList};
findMyParcelLocker( PersonLocation , [], ClosestLocation) ->
  {PersonLocation, ClosestLocation};
findMyParcelLocker(PersonLocation, [LockerLocations | Tail], {}) ->
  findMyParcelLocker(PersonLocation, Tail, LockerLocations);
findMyParcelLocker(PersonLocation, [ LockerLocation | Tail], ClosestLocation)  ->
  case findDistancePowered2(PersonLocation, LockerLocation) < findDistancePowered2(PersonLocation, ClosestLocation)  of
    true -> findMyParcelLocker(PersonLocation, Tail, LockerLocation);
    false -> findMyParcelLocker(PersonLocation, Tail, ClosestLocation)
  end.

test(ClientsNumber, LockersNumber) ->
  {ClientsList, LockersList} = prepareVariables(ClientsNumber, LockersNumber),
  io:format("Test made for: ~n   ClientsNumber: ~p ~n   LockersNumber: ~p ~n", [ClientsNumber, LockersNumber]),
  parallelVersion(ClientsList, LockersList),
  semiParallelVersion(ClientsList, LockersList),
  sequentialVersion(ClientsList, LockersList).

result(List, ClientsCounter, SecStart, SecEnd, fullParallel) when (ClientsCounter == 0)->
%%  io:format("~n ~p ~n",[List]),
  io:format("Full Parallel version took: ~p seconds~n", [SecEnd - SecStart]),
  List;
result(List, ClientsCounter, SecStart, SecEnd, semiParallel) when (ClientsCounter == 0)->
%%  io:format("~n ~p ~n",[List]),
  io:format("Semi-Parallel version took: ~p seconds~n", [SecEnd - SecStart]),
  List;
result(List, ClientsCounter, SecStart, _, _) ->
  receive
    {ClientAndClosest, Type}->
      NewList = [ ClientAndClosest | List ],
      {_, NewSecEnd ,_} = now(),
      result(NewList, ClientsCounter -1, SecStart, NewSecEnd, Type)
  end.

%% SEQUENTIAL
sequentialVersion(ClientsList, LockersList) ->
  {_, SecStart, _} = now(),
  ResultList = [ findMyParcelLocker(Client, LockersList) || Client <- ClientsList],
  {_, SecEnd, _} = now(),
  io:format("Sequential version took: ~p seconds ~n", [SecEnd - SecStart]).
%%  ResultList.


%% FULL PARALLEL
parallelVersion(ClientsList, LockersList) ->
  ClientsCounter = length(ClientsList),
  {_, SecStart, _} = now(),
  Result = spawn(fun() -> result([], ClientsCounter, SecStart, 0, fullParallel) end),
  register(fullParallelResult, Result),
  parallelVersion(ClientsList, LockersList, helpFunction).

parallelVersion([], _, helpFunction) -> ok;
parallelVersion([ClientLocation | RestClients], LockersList, helpFunction) ->
  FindLockerProcess = spawn(fun() -> parallelFind(LockersList) end),
  FindLockerProcess ! ClientLocation,
  parallelVersion(RestClients, LockersList, helpFunction).

parallelFind(LockersList) ->
  receive
    ClientLocation ->
      fullParallelResult ! {findMyParcelLocker(ClientLocation, LockersList), fullParallel}
  end.


%% SEMI-PARALLEL
semiParallelVersion(ClientsList, LockersList) ->
  ClientsCounter = length(ClientsList),
  ResultListLength = round(ClientsCounter / 8),
  ClientsListOfLists = splitList(ClientsList, ResultListLength, []),
  {_, SecStart, _} = now(),
  Result = spawn(fun() -> result([], ClientsCounter, SecStart, 0, semiParallel) end),
  register(semiParallelResult, Result),
  semiParallelVersion(ClientsListOfLists, LockersList, helpFunction).

semiParallelVersion([], _, helpFunction) -> ok;
semiParallelVersion([PartOfClientsList | Tail], LockersList, helpFunction) ->
  Fun = fun() -> findPartOfClients(LockersList) end,
  Process = spawn(Fun),
  Process ! PartOfClientsList,
  semiParallelVersion(Tail, LockersList, helpFunction).

findPartOfClients(LockersList) ->
  receive
    [ClientLocation | RestClients] ->
      semiParallelResult ! {findMyParcelLocker(ClientLocation, LockersList), semiParallel},
      self() ! RestClients,
      findPartOfClients(LockersList)
  end.

splitList([], _, ResultList) ->
  ResultList;
splitList(OriginList, Length, ResultList) when Length > length(OriginList) ->
  NewResult = [OriginList | ResultList],
  splitList([], Length, NewResult);
splitList(OriginList, Length, ResultList) ->
  {List, Tail} = lists:split(Length, OriginList),
  NewResult = [List | ResultList],
  splitList(Tail, Length, NewResult).


