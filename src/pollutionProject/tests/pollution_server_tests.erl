%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2020 20:16
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("jasiek").

-include_lib("eunit/include/eunit.hrl").
-include("pollutionRecords.hrl").
-export([createSampleMonitor2/0]).

simple_test() ->
  ?assert(true).


startServer() ->
  pollution_server:start().
%%  timer:sleep(10).

startServer(sample2) ->
  Monitor = createSampleMonitor2(),
  pollution_server:start(Monitor).
%%  timer:sleep(10).

stopServer() ->
  pollution_server:stop(),
  timer:sleep(10).

createSampleMonitor2() ->
  #monitor{stations =
  [
    #station{name = "St0", location = {0, 0},
      measurements = #{
        {{{2020, 4, 6}, {2, 37, 45}}, "PM10"} => 90,
        {{{2020, 4, 7}, {2, 37, 45}}, "PM10"} => 80,
        {{{2020, 4, 6}, {2, 47, 45}}, "PM10"} => 70,
        {{{2020, 4, 6}, {2, 37, 45}}, "PM25"} => 60,
        {{{2020, 4, 7}, {2, 37, 45}}, "PM25"} => 50,
        {{{2020, 4, 6}, {2, 47, 45}}, "PM25"} => 40,
        {{{2020, 4, 6}, {2, 57, 45}}, "PM25"} => 32,
        {{{2020, 4, 7}, {2, 37, 45}}, "Tmp"} => 20,
        {{{2020, 4, 6}, {2, 37, 45}}, "Tmp"} => 10
      }},
    #station{name = "St1", location = {1, 1},
      measurements = #{
        {{{2020, 4, 6}, {2, 37, 45}}, "PM10"} => 50,
        {{{2020, 4, 7}, {2, 37, 45}}, "PM25"} => 100,
        {{{2020, 4, 8}, {2, 37, 45}}, "Tmp"} => 150
      }},
    #station{name = "St2", location = {2, 2}, measurements = #{}},
    #station{name = "St3", location = {3, 3}, measurements = #{}}
  ]}.


start_stop_test() ->
  startServer(),
  ?assertNotEqual(undefined, whereis(pollutionServer)),
  stopServer(),
  ?assertEqual(undefined, whereis(pollutionServer)).

start_2_test() ->
  Monitor = createSampleMonitor2(),
  pollution_server:start(Monitor),
  ?assertNotEqual(undefined, whereis(pollutionServer)),
  stopServer().

addStation_1_test() -> %% can add new station
  startServer(sample2),
  Current = pollution_server:addStation("St0", {0, 0}),
  [
    ?_assertEqual(ok, Current)
  ],
  stopServer().

addStation_2_test() -> %% can't add already existing station
  startServer(sample2),
  Current = pollution_server:addStation("St0", {0, 0}),
  [
    ?_assertEqual(ok, Current)
  ],
  stopServer().

addMeasurement_test() -> %% everything goes well
  startServer(sample2),
  Current1 = pollution_server:addMeasurement("St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}, 20), %% everything goes well
  Current2 = pollution_server:addMeasurement("St55", "PM10", {{2020, 4, 6}, {2, 37, 45}}, 20),  %% add measurement to nonExisting Station
  Current3 = pollution_server:addMeasurement("St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}, 20), %% add already existing measurement
  [
    ?_assertEqual(ok, Current1),
    ?_assertEqual({error, notFoundStationWithThatName}, Current2),
    ?_assertEqual(ok, Current3)
  ],
  stopServer().


removeMeasurement_test() ->
  startServer(sample2),
  Current1 = pollution_server:removeMeasurement("St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}),  %% everything goes well
  Current2 = pollution_server:removeMeasurement("St0", "PM11", {{2020, 4, 6}, {2, 37, 45}}),  %% try to remove unexisting measurement
  Current3 = pollution_server:removeMeasurement("St10", "PM10", {{2020, 4, 6}, {2, 37, 45}}), %% try remove sth from nonExisting Station
  [
    ?_assertEqual(ok, Current1),
    ?_assertEqual(ok, Current2),
    ?_assertEqual({error, notFoundStationWithThatName}, Current3)
  ],
  stopServer().


getValueOfMeasurement_test() ->
  startServer(sample2),
  Expected1 = 90,
  Expected2 = 20,

  Current1 = pollution_server:getValueOfMeasurement("St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}),
  Current2 = pollution_server:getValueOfMeasurement("St0", "Tmp", {{2020, 4, 7}, {2, 37, 45}}),
  [
    ?_assertEqual(Expected1, Current1),
    ?_assertEqual(Expected2, Current2)
  ],
  stopServer().


getStationMeanByStationName_test() ->
  startServer(sample2),
  Expected1 = 80.0,
  Expected2 = 45.5,
  Expected3 = 15.0,
  Expected4 = {error, noSuchTypeOfMeasurement},

  Current1 = pollution_server:getStationMeanByStationName("St0", "PM10"),
  Current2 = pollution_server:getStationMeanByStationName("St0", "PM25"),
  Current3 = pollution_server:getStationMeanByStationName("St0", "Tmp"),
  Current4 = pollution_server:getStationMeanByStationName("St2", "Tmp"),
  [
    ?_assertEqual(Expected1, Current1),
    ?_assertEqual(Expected2, Current2),
    ?_assertEqual(Expected3, Current3),
    ?_assertEqual(Expected4, Current4)
  ],
  stopServer().


getDailyMean_test() ->
  startServer(sample2),
  Expected1 = 70.0, %% PM10 2020-04-06
  Expected2 = {error, noThisTypeOfMeasurementInThisDay}, %% WWW  2020-04-06 -> nie ma takiego typu
  Expected3 = {error, noThisTypeOfMeasurementInThisDay}, %% PM25 2020-04-09 -> nie ma w takim dniu

  Current1 = pollution_server:getDailyMean("PM10", {{2020, 4, 6}, {}}),
  Current2 = pollution_server:getDailyMean("WWW", {{2020, 4, 6}, {}}),
  Current3 = pollution_server:getDailyMean("PM25", {{2020, 4, 9}, {}}),
  [
    ?_assertEqual(Expected1, Current1),
    ?_assertEqual(Expected2, Current2),
    ?_assertEqual(Expected3, Current3)
  ],
  stopServer().


getStationsWithHighestMeanMeasurement_test() ->
  startServer(sample2),
  Expected1 = 80.0,
  Expected2 = 100.0,
  Expected3 = [],

  Current1 = pollution_server:getStationsWithHighestMeanMeasurements("PM10"),
  Current2 = pollution_server:getStationsWithHighestMeanMeasurements("PM25"),
  Current3 = pollution_server:getStationsWithHighestMeanMeasurements("WWW"),
  [
    ?_assertEqual(Expected1, Current1),
    ?_assertEqual(Expected2, Current2),
    ?_assertEqual(Expected3, Current3)
  ],
  stopServer().


getStationsWithMeanMeasurementsOverLimit_test() ->
  startServer(sample2),

  Expected1 = 80.0,
  Expected2 = 50.0,
  Expected3 = [],
  Expected5 = [],

  Current1 = pollution_server:getStationsWithMeanMeasurementsOverLimit("PM10", 70),
  Current2 = pollution_server:getStationsWithMeanMeasurementsOverLimit("PM10", 40),
  Current3 = pollution_server:getStationsWithMeanMeasurementsOverLimit("WWW", 70),

  Current5 = pollution_server:getStationsWithMeanMeasurementsOverLimit("PM10", 120),
  [
    ?_assertEqual(Expected1, Current1),
    ?_assertEqual(Expected2, Current2),
    ?_assertEqual(Expected3, Current3),
    ?_assertEqual(Expected5, Current5)
  ],
  stopServer().


