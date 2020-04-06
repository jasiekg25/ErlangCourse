%%%-------------------------------------------------------------------
%%% @author jasiek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2020 14:24
%%%-------------------------------------------------------------------
-author("jasiek").

-record(station, {name, location, measurements}). %% measurements as Map (Data_time, Type) => Value
%%-record(measurement, {date_time, type, value}).
-record(monitor, {stations}).  %% Stations as List
