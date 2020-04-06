%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2020 22:52
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("jasiek").

%% API
%%-export([]).
-export([start/0, stop/0, getMonitor/0, addStation/2, addMeasurement/4, removeMeasurement/3, getStationsWithMeanMeasurementsOverLimit/2, getStationWithHighestMeanMeasurements/1, getDailyMean/2, getStationMeanByStationName/2, getValueOfMeasurement/3]).

start() ->
  register(pollutionServer, spawn(fun() -> init() end)).

stop() ->
  pollutionServer ! {request, self(), terminate}.

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {request, Pid, addStation, Args} ->
      NewMonitor = pollution:addStation(Monitor, lists:nth(1, Args), lists:nth(2, Args)),
      Pid ! {reply, ok},
      loop(NewMonitor);
    {request, Pid, addMeasurement, Args} ->
      Result = pollution:addMeasurement(Monitor, lists:nth(1, Args), lists:nth(2, Args), lists:nth(3, Args), lists:nth(4, Args)),
      case Result of
        {error, Msg} ->
          Pid ! {reply, {error, Msg}},
          loop(Monitor);
        NewMonitor ->
          Pid ! {reply, ok},
          loop(NewMonitor)
      end;
    {request, Pid, removeMeasurement, Args} ->
      Result = pollution:removeMeasurement(Monitor, lists:nth(1, Args), lists:nth(2, Args), lists:nth(3, Args)),
      case Result of
        {error, Msg} ->
          Pid ! {reply, {error, Msg}},
          loop(Monitor);
        NewMonitor ->
          Pid ! {reply, ok},
          loop(NewMonitor)
      end;
    {request, Pid, getValueOfMeasurement, Args} ->
      Value = pollution:getValueOfMeasurement(Monitor, lists:nth(1, Args), lists:nth(2, Args), lists:nth(3, Args)),
      Pid ! {reply, Value},
      loop(Monitor);
    {request, Pid, getStationMeanByStationName, Args} ->
      {_, _, StationMeanValue} = pollution:getStationMeanByStationName(Monitor, lists:nth(1, Args), lists:nth(2, Args)),
      Pid ! {reply, StationMeanValue},
      loop(Monitor);
    {request, Pid, getDailyMean, Args} ->
      DailyMeanValue = pollution:getDailyMean(Monitor, lists:nth(1, Args), lists:nth(2, Args)),
      Pid ! {reply, DailyMeanValue},
      loop(Monitor);
    {request, Pid, getStationWithHighestMeanMeasurements, Args} ->
      List = pollution:getStationWithHighestMeanMeasurements(Monitor, lists:nth(1, Args)),
      Pid ! {reply, List},
      loop(Monitor);
    {request, Pid, getStationsWithMeanMeasurementsOverLimit, Args} ->
      List = pollution:getStationsWithMeanMeasurementsOverLimit(Monitor, lists:nth(1, Args), lists:nth(2, Args)),
      Pid ! {reply, List},
      loop(Monitor);
    {request, Pid, getMonitor, _} ->
      Pid ! {reply, Monitor},
      loop(Monitor);
    {request, Pid, terminate} ->
      Pid ! {reply, ok}

  end.

%% Client's API

call(Message, Args) ->
  pollutionServer ! {request, self(), Message, Args},
  receive
    {reply, Reply} ->
      io:format("~p : ~p~n", [Message, Reply]),
      Reply
  end.

getMonitor() ->
  call(getMonitor, []).

addStation(StationName, StationLocation) ->
  call(addStation, [StationName, StationLocation]).

addMeasurement(StationName, Type, Date, Value) ->
  call(addMeasurement, [StationName, Type, Date, Value]).

removeMeasurement(StationName, Type, Date) ->
  call(removeMeasurement, [StationName, Type, Date]).

getValueOfMeasurement(StationName, Type, Date) ->
  call(getValueOfMeasurement, [StationName, Type, Date]).

getStationMeanByStationName(StationName, Type) ->
  call(getStationMeanByStationName, [StationName, Type]).

getDailyMean(Type, Date) ->
  call(getDailyMean, [Type, Date]).

getStationWithHighestMeanMeasurements(Type) ->
  call(getStationWithHighestMeanMeasurements, [Type]).

getStationsWithMeanMeasurementsOverLimit(Type, Limit) ->
  call(getStationsWithMeanMeasurementsOverLimit, [Type, Limit]).
