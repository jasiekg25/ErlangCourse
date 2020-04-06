%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2020 13:15
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("jasiek").

-include_lib("eunit/include/eunit.hrl").
-include("pollutionRecords.hrl").

-export([createSampleMonitor2/0]).
my_test_() ->
  [
    test_createMonitor(),
    test_addStation_1(),
    test_addStation_2(),
    test_addStation_3(),
    test_addMeasurement(),
    test_getStationByStationName(),
    test_removeMeasurement(),
    test_getValueOfMeasurement(),
    test_getStationMeanByStationName(),
    test_getDailyMean(),
    test_getStationsWithHighestMeanMeasurement(),
    test_getStationsWithMeanMeasurementsOverLimit(),
    ?_assertError(badarith, 1 / 0)
  ].

createSampleMonitor1() ->
    #monitor{stations =
    [
      #station{name = "St0", location = {0, 0}, measurements = #{}},
      #station{name = "St1", location = {1, 1}, measurements = #{}},
      #station{name = "St2", location = {2, 2}, measurements = #{}},
      #station{name = "St3", location = {3, 3}, measurements = #{}}
    ]}.


createSampleMonitor2() ->
    #monitor{stations =
    [
      #station{name = "St0", location = {0, 0},
        measurements = #{
          {{{2020, 4, 6}, {2, 37, 45}}, "PM10"} => 90,
          {{{2020, 4, 7}, {2, 37, 45}}, "PM10"} => 80,
          {{{2020, 4, 6}, {2, 37, 45}}, "PM10"} => 70,
          {{{2020, 4, 6}, {2, 37, 45}}, "PM25"} => 60,
          {{{2020, 4, 7}, {2, 37, 45}}, "PM25"} => 50,
          {{{2020, 4, 6}, {2, 37, 45}}, "PM25"} => 40,
          {{{2020, 4, 6}, {2, 37, 45}}, "PM25"} => 32,
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

test_createMonitor() ->
  Expected = #monitor{stations = []},
  Current = pollution:createMonitor(),
  ?_assertEqual(Expected, Current).

test_addStation_1() -> %% can add new station
  Monitor = pollution:createMonitor(),
  Current = pollution:addStation(Monitor, "St0", {0, 0}),
  Expected = #monitor{stations =
  [#station{name = "St0", location = {0, 0}, measurements = #{}}]},
  ?_assertEqual(Expected, Current).

test_addStation_2() -> %% can't add already existing station
  Expected = createSampleMonitor1(),
  Current = pollution:addStation(Expected, "St0", {0, 0}),
  ?_assertEqual(Expected, Current).

test_addStation_3() -> %% can add more then 1 station
  Expected = createSampleMonitor1(),
  M = pollution:createMonitor(),
  M0 = pollution:addStation(M, "St3", {3, 3}),
  M1 = pollution:addStation(M0, "St2", {2, 2}),
  M2 = pollution:addStation(M1, "St1", {1, 1}),
  Current = pollution:addStation(M2, "St0", {0, 0}),
  ?_assertEqual(Expected, Current).

test_getStationByStationName() ->
  Expected1 = {found, #station{name = "St0", location = {0, 0}, measurements = #{}}},
  Expected2 = {error, notFoundStationWithThatName},

  Monitor = createSampleMonitor1(),
  Current1 = pollution:getStationByStationName(Monitor, "St0"),
  Current2 = pollution:getStationByStationName(Monitor, "St10"),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2).

test_addMeasurement() -> %% everything goes well
  ExpectedMonitor =
    #monitor{stations =
    [
      #station{name = "St0", location = {0, 0}, measurements = #{{{{2020, 4, 6}, {2, 37, 45}}, "PM10"} => 20}},
      #station{name = "St1", location = {1, 1}, measurements = #{}},
      #station{name = "St2", location = {2, 2}, measurements = #{}},
      #station{name = "St3", location = {3, 3}, measurements = #{}}
    ]},
  Expected1 = ExpectedMonitor, %% everything goes well
  Expected2 = {error, notFoundStationWithThatName}, %% add measurement to nonExisting Station
  Expected3 = ExpectedMonitor, %% add already existing measurement

  Monitor = createSampleMonitor1(),
  Current1 = pollution:addMeasurement(Monitor, "St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}, 20),
  Current2 = pollution:addMeasurement(Monitor, "St55", "PM10", {{2020, 4, 6}, {2, 37, 45}}, 20),
  Current3 = pollution:addMeasurement(Monitor, "St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}, 20),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2),
  ?_assertEqual(Expected3, Current3).

test_removeMeasurement() ->
  Monitor = #monitor{stations =
  [
    #station{name = "St0", location = {0, 0}, measurements = #{{{{2020, 4, 6}, {2, 37, 45}}, "PM10"} => 20}},
    #station{name = "St1", location = {1, 1}, measurements = #{}},
    #station{name = "St2", location = {2, 2}, measurements = #{}},
    #station{name = "St3", location = {3, 3}, measurements = #{}}
  ]},

  Expected1 = createSampleMonitor1(), %% everything goes well
  Expected2 = Monitor, %% try to remove unexisting measurement
  Expected3 = {error, notFoundStationWithThatName}, %% try remove sth from nonExisting Station

  Current1 = pollution:removeMeasurement(Monitor, "St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}),
  Current2 = pollution:removeMeasurement(Monitor, "St0", "PM11", {{2020, 4, 6}, {2, 37, 45}}),
  Current3 = pollution:removeMeasurement(Monitor, "St10", "PM10", {{2020, 4, 6}, {2, 37, 45}}),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2),
  ?_assertEqual(Expected3, Current3).


test_getValueOfMeasurement() ->
  Monitor = createSampleMonitor2(),
  Expected1 = 90,
  Expected2 = 20,

  Current1 = pollution:getValueOfMeasurement(Monitor, "St0", "PM10", {{2020, 4, 6}, {2, 37, 45}}),
  Current2 = pollution:getValueOfMeasurement(Monitor, "St0", "Tmp", {{2020, 4, 7}, {2, 37, 45}}),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2).

test_getStationMeanByStationName() ->
  Monitor = createSampleMonitor2(),
  Expected1 = 80,
  Expected2 = 45,
  Expected3 = 15,
  Expected4 = {error, noSuchTypeOfMeasurement},

  {_, _, Current1} = pollution:getStationMeanByStationName(Monitor, "St0", "PM10"),
  {_, _, Current2} = pollution:getStationMeanByStationName(Monitor, "St0", "PM25"),
  {_, _, Current3} = pollution:getStationMeanByStationName(Monitor, "St0", "Tmp"),
  {_, _, Current4} = pollution:getStationMeanByStationName(Monitor, "St2", "Tmp"),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2),
  ?_assertEqual(Expected3, Current3),
  ?_assertEqual(Expected4, Current4).

test_getDailyMean() ->
  Monitor = createSampleMonitor2(),
  Expected1 = 70, %% PM10 2020-04-06
  Expected2 = {error, noThisTypeOfMeasurementInThisDay}, %% WWW  2020-04-06 -> nie ma takiego typu
  Expected3 = {error, noThisTypeOfMeasurementInThisDay}, %% PM25 2020-04-09 -> nie ma w takim dniu

  Current1 = pollution:getDailyMean(Monitor, "PM10", {{2020, 4, 6}, {}}),
  Current2 = pollution:getDailyMean(Monitor, "WWW", {{2020, 4, 6}, {}}),
  Current3 = pollution:getDailyMean(Monitor, "PM25", {{2020, 4, 9}, {}}),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2),
  ?_assertEqual(Expected3, Current3).

test_getStationsWithHighestMeanMeasurement() ->
  Monitor = createSampleMonitor2(),

  Expected1 = 80,
  Expected2 = 100,
  Expected3 = [],

  [{_, _, Current1} | []] = pollution:getStationsWithHighestMeanMeasurements(Monitor, "PM10"),
  [{_, _, Current2} | []] = pollution:getStationsWithHighestMeanMeasurements(Monitor, "PM25"),
  Current3 = pollution:getStationsWithHighestMeanMeasurements(Monitor, "WWW"),

  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2),
  ?_assertEqual(Expected3, Current3).

test_getStationsWithMeanMeasurementsOverLimit() ->
  Monitor = createSampleMonitor2(),

  Expected1 = 80,
  Expected2 = 50,
  Expected3 = [],
  Expected4 = 2,
  Expected5 = [],

  [{_, _, Current1} | []] = pollution:getStationsWithMeanMeasurementsOverLimit(Monitor, "PM10", 70),
  [{_, _, Current2} | _] = pollution:getStationsWithMeanMeasurementsOverLimit(Monitor, "PM10", 40),
  Current3 = pollution:getStationsWithMeanMeasurementsOverLimit(Monitor, "WWW", 70),
  List = pollution:getStationsWithMeanMeasurementsOverLimit(Monitor, "PM10", 10),
  Current4 = length(List),
  Current5 = pollution:getStationsWithMeanMeasurementsOverLimit(Monitor, "PM10", 120),
  ?_assertEqual(Expected1, Current1),
  ?_assertEqual(Expected2, Current2),
  ?_assertEqual(Expected3, Current3),
  ?_assertEqual(Expected4, Current4),
  ?_assertEqual(Expected5, Current5).


