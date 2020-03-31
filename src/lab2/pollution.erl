%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Mar 2020 04:54
%%%-------------------------------------------------------------------
-module(pollution).
-author("jasiek").

%% API
-compile(export_all).
%%-export([createMonitor/0, addStation/3, addMeasurement/5, removeMeasurement/4, getValueOfMeasurement/4]).


-record(station, {name, location, measurements}). %% measurements as Map (Data_time, Type) => Value
%%-record(measurement, {date_time, type, value}).
-record(monitor, {stations}).  %% Stations as List

%% createMonitor() -> monitor`
%% addStation(StationName, {X, Y}, OldMonitor) -> monitor
%% addValue(StationName, Date, Type, Value) -> monitor
%% removeValue(StationName, Date, Type) -> monitor
%% getOneValue(StationName, Date, Type) -> value
%% getStationMean(StationName, Type) -> MeanValue
%% getDailyMean(Date, Type) -> MeanValue
%%
%%getWorstStation(Type, date) -> StationName
%%getOverLimitStations(Type, Date) -> monitor

createMonitor() ->
  Monitor = #monitor{stations = []},
  Monitor.

addStation(OldMonitor, StationName, {X, Y}) ->
  Stations = OldMonitor#monitor.stations,
  NewStation = #station{name = StationName, location = {X, Y}, measurements = #{}},
  NewMonitor = #monitor{stations = [NewStation | Stations]},
  NewMonitor.

getStationByStationName(Monitor, StationName) ->
  Stations = Monitor#monitor.stations,
  findStationByName(StationName, Stations).

findStationByName(StationName, [#station{location = {X, Y}, name = StationName, measurements = M} | _]) ->
  {found, #station{location = {X, Y}, name = StationName, measurements = M}};
findStationByName(StationName, [_ | Stations]) ->
  findStationByName(StationName, Stations);
findStationByName(_, []) ->
  {err, notFound}.


addMeasurement(Monitor, StationName, Type, Date, Value) -> %% addValue
  Station = getStationByStationName(Monitor, StationName),
  case Station of
    {err, notFound} ->
      {err, nonExistentStation};
    {err, _} ->
      {err, sthWrongHappened};
    {found, FoundStation} ->
      MeasurementsMap = FoundStation#station.measurements,
      NewMeasurementsMap = MeasurementsMap#{{Date, Type} => Value},
      NewStation = FoundStation#station{measurements = NewMeasurementsMap},
      NotEqual = fun(Station) -> not (Station =:= FoundStation) end,
      RestOfStation = lists:filter(NotEqual, Monitor#monitor.stations),
      NewMonitor = #monitor{stations = [NewStation | RestOfStation]},
      NewMonitor
  end.

removeMeasurement(Monitor, StationName, Type, Date) ->
  Station = getStationByStationName(Monitor, StationName),
  case Station of
    {err, notFound} ->
      {err, nonExistentStation};
    {err, _} ->
      {err, sthWrongHappened};
    {found, FoundStation} ->
      MeasurementsMap = FoundStation#station.measurements,
      NotEqualMap = fun(Key, _) -> not (Key == {Date, Type}) end,
      NewMeasurementsMap = maps:filter(NotEqualMap, MeasurementsMap), %% without removedMeasurement
      NewStation = FoundStation#station{measurements = NewMeasurementsMap},
      NotEqual = fun(Station) -> not (Station =:= FoundStation) end,
      RestOfStation = lists:filter(NotEqual, Monitor#monitor.stations),
      NewMonitor = #monitor{stations = [NewStation | RestOfStation]},
      NewMonitor
  end.

getValueOfMeasurement(Monitor, StationName, Type, Date) ->
  Station = getStationByStationName(Monitor, StationName),
  case Station of
    {err, notFound} ->
      {err, nonExistentStation};
    {err, _} ->
      {err, sthWrongHappened};
    {found, FoundStation} ->
      MeasurementsMap = FoundStation#station.measurements,
      maps:get({Date, Type}, MeasurementsMap)
  end.


getStationMeanByStationName(Monitor, StationName, Type) ->
  Station = getStationByStationName(Monitor, StationName),
  case Station of
    {err, notFound} ->
      {err, nonExistentStation};
    {err, _} ->
      {err, unknownError};
    {found, FoundStation} ->
      getStationMeanByStation(Monitor, FoundStation, Type)
  end.


getStationMeanByStation(Monitor, Station, Type) ->
  MeasurementsMap = Station#station.measurements,
  FindMeasurementsWithType = fun({_, Type1}, _) -> Type1 == Type end,
  MeasurementsWithType = maps:values(maps:filter(FindMeasurementsWithType, MeasurementsMap)),
  TotalValue = lists:foldl(fun(A, B) -> A + B end, 0, MeasurementsWithType),
  TotalNumber = length(MeasurementsWithType),
  case TotalNumber == 0 of
    true -> MeanValue = 0;
    false -> MeanValue = TotalValue / TotalNumber
  end,
  {Monitor, Station, MeanValue}.


getDailyMean(Monitor, Type, {Date, _}) ->
  FindCorrectMeasurements = fun({{Date1, _}, Type1}, _) -> ((Date1 == Date) and (Type1 == Type)) end,
  Stations = Monitor#monitor.stations,

  ValuesForEachStation = fun(#station{measurements = Map}) ->
    lists:foldl(fun(A, B) -> A + B end, 0, maps:values(maps:filter(FindCorrectMeasurements, Map)))
                         end,
  NumberOfMeasurementsForEachStation = fun(#station{measurements = Map}) ->
    length(maps:values(maps:filter(FindCorrectMeasurements, Map)))
                                       end,

  MappedValues = lists:map(ValuesForEachStation, Stations),
  MappedNumbers = lists:map(NumberOfMeasurementsForEachStation, Stations),

  ValueTotal = lists:foldl(fun(A, B) -> A + B end, 0, MappedValues),
  NumberTotal = lists:foldl(fun(A, B) -> A + B end, 0, MappedNumbers),
  case NumberTotal == 0 of
    true -> 0;
    false -> ValueTotal / NumberTotal
  end.


getStationWithHighestMeanMeasurements(Monitor, Type) ->
  Stations = Monitor#monitor.stations,
  Fun = fun(Station) -> getStationMeanByStation(Monitor, Station, Type)  end,
  StationsMeanList = lists:map(Fun, Stations),
  StationsWithHighestMean = findHighest(StationsMeanList),
  StationsWithHighestMean.


findHighest([{M1, S1, Value} | Tail]) ->
  findHighest([{M1, S1, Value} | Tail], []).
findHighest([], HighestList) ->
  HighestList;
findHighest([{M1, S1, Value} | Tail], []) ->
  findHighest(Tail, [{M1, S1, Value}]);
findHighest([{M1, S1, Value} | Tail], [{_, _, MaxValue} | _]) when Value > MaxValue ->
  findHighest(Tail, [{M1, S1, Value}]);
findHighest([{_, _, Value} | Tail], [{MM, SM, MaxValue} | List]) when Value < MaxValue ->
  findHighest(Tail, [{MM, SM, MaxValue} | List]);
findHighest([{M1, S1, Value} | Tail], [{MM, SM, MaxValue} | List]) when Value == MaxValue ->
  List2 = [{MM, SM, MaxValue} | List],
  findHighest(Tail, [{M1, S1, Value} | List2]).

getStationsWithMeanMeasurementsOverLimit(Monitor, Type, Limit) ->
  Stations = Monitor#monitor.stations,
  Fun = fun(Station) -> getStationMeanByStation(Monitor, Station, Type) end,
  StationsMeanList = lists:map(Fun, Stations),
  StationsWithMeanMeasurementsOverLimit = findHigher(StationsMeanList, Limit),
  StationsWithMeanMeasurementsOverLimit.


findHigher(List, Limit) ->
  findHigher(List, Limit, []).
findHigher([], _, ResultList) ->
  ResultList;
findHigher([{M1, S1, V1} | Tail], Limit, []) when V1 > Limit ->
  findHigher(Tail, Limit, [{M1, S1, V1}]);
findHigher([{_, _, V1} | Tail], Limit, []) when V1 =< Limit ->
  findHigher(Tail, Limit, []);
findHigher([{M1, S1, V1} | Tail], Limit, ResultList) when V1 > Limit ->
  findHigher(Tail, Limit, [{M1, S1, V1} | ResultList]);
findHigher([{_, _, V1} | Tail], Limit, ResultList) when V1 =< Limit ->
  findHigher(Tail, Limit, ResultList).
