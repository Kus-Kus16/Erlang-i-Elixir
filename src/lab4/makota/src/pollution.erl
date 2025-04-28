-module(pollution).

-export([create_monitor/0, add_station/3, get_one_value/4, add_value/5, remove_value/4,
    get_station_min/3, get_daily_mean/3, get_moving_mean/4, get_example_data/0, get_station_mean/3]).

-record(monitor, { coordinates_map = #{}, name_map = #{} }).
-record(station, { coordinates, name, readings = [] } ).
-record(reading, { datetime, type, value } ).

% coordinates: {number, number}
% datetime: {date, time}

create_monitor() ->
    #monitor{}.

add_station(Name, Coordinates, Monitor) ->
    NameMap = Monitor#monitor.name_map,
    CoordinatesMap = Monitor#monitor.coordinates_map,

    Already_Exists = maps:is_key(Name, NameMap)
        orelse maps:is_key(Coordinates, CoordinatesMap),

    case Already_Exists of
        true -> {error, station_alreadyexists};
        _ ->
            Station = #station{ coordinates = Coordinates, name = Name },
            Monitor#monitor{ coordinates_map = CoordinatesMap#{ Coordinates => Station },
                name_map = NameMap#{ Name => Station } }
    end.

add_value(StationIdentifier, Datetime, Type, Value, Monitor) ->
    Station = get_station(StationIdentifier, Monitor),

    case Station of
        {error, _} = Error -> Error;
        Station ->
            case get_one_value(StationIdentifier, Datetime, Type, Monitor) of
                {error, value_doesnotexist} ->
                    Reading = #reading{ datetime = Datetime, type = Type, value = Value },
                    NewStation = Station#station{readings = Station#station.readings ++ [Reading]},
                    update_station(StationIdentifier, NewStation, Monitor);
                _ -> {error, reading_alreadyexists}
            end
    end.

remove_value(StationIdentifier, Datetime, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor),

    case Station of
        {error, _} = Error -> Error;
        Station ->
            NewReadings = [ R || R <- Station#station.readings,
                 not (R#reading.datetime =:= Datetime andalso R#reading.type =:= Type) ],

            case NewReadings == Station#station.readings of
                true -> {error, reading_doesnotexist};
                _ ->
                    NewStation = Station#station{readings = NewReadings},
                    update_station(StationIdentifier, NewStation, Monitor)
            end
    end.

get_one_value(StationIdentifier, Datetime, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor),

    case Station of
        {error, _} = Error -> Error;
        Station ->
            case [ R#reading.value || R <- Station#station.readings,
                    R#reading.datetime =:= Datetime, R#reading.type =:= Type ] of

                [] -> {error, value_doesnotexist};
                [H | _] -> H
            end
    end.

get_station_min(StationIdentifier, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor),

    case Station of
        {error, _} = Error -> Error;
        Station ->
            Readings = [ R#reading.value || R <- Station#station.readings,
                R#reading.type =:= Type ],

            lists:min(Readings)
    end.

get_station_mean(StationIdentifier, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor),

    case Station of
        {error, _} = Error -> Error;
        Station ->
            Readings = [ R#reading.value || R <- Station#station.readings,
                R#reading.type =:= Type ],

            case Readings of
                [] -> {error, no_readings};
                Readings -> lists:sum(Readings) / length(Readings)
            end
    end.

get_daily_mean(Date, Type, Monitor) ->
    Readings = lists:foldl(fun (Station, Acc) -> Station#station.readings ++ Acc end, [],
        maps:values(Monitor#monitor.coordinates_map)),

    FilteredReadings = [ R#reading.value || R <- Readings,
        R#reading.type =:= Type, element(1, R#reading.datetime) =:= Date ],

    case FilteredReadings of
        [] -> {error, no_readings};
        FilteredReadings -> lists:sum(FilteredReadings) / length(FilteredReadings)
    end.

%% Średnia krocząca z wartości parametru danego typu w danym dniu na danej stacji.
%% Jest to średnia ważona z pomiarów z ostatniej doby, gdzie wartość bieżąca ma wagę 24,
%% wartość sprzed godziny wagę 23 itd.

get_moving_mean(StationIdentifier, Date, Type, Monitor) ->
    Station = get_station(StationIdentifier, Monitor),

    case Station of
        {error, _} = Error -> Error;
        Station ->
            Readings = [ R || R <- Station#station.readings, R#reading.type =:= Type,
                element(1, R#reading.datetime) =:= Date],

            case Readings of
                [] -> {error, no_readings};
                _ ->
                    Sorted = lists:reverse(lists:sort(Readings)),
                    WeightedList = get_weighted_values(Sorted, hd(Sorted)),

                    {Nom, Denom} = lists:foldl(fun ({W, V}, {Nom, Denom}) -> {Nom + W * V, Denom + W} end,
                        {0, 0}, WeightedList),

                    Nom / Denom
            end
    end.

%% private
get_station(StationIdentifier, Monitor) ->
    Error = {error, station_doesnotexist},

    case StationIdentifier of
        {station, _, Name, _} -> get_station(Name, Monitor);
        [_|_] -> maps:get(StationIdentifier, Monitor#monitor.name_map, Error);
        {_,_} -> maps:get(StationIdentifier, Monitor#monitor.coordinates_map, Error);
        _ -> {error, bad_stationidentifier_format}
    end.

%% private
update_station(OldStationIdentifier, NewStation, Monitor) ->
    OldStation = get_station(OldStationIdentifier, Monitor),

    case OldStation of
        {error, _} -> {error, oldstation_doesnotexist};
        OldStation ->
            Name = OldStation#station.name,
            Coordinates = OldStation#station.coordinates,

            NamesMap = maps:update(Name, NewStation, Monitor#monitor.name_map),
            CoordinatesMap = maps:update(Coordinates, NewStation, Monitor#monitor.coordinates_map),

            #monitor{ name_map = NamesMap, coordinates_map = CoordinatesMap }
    end.

%% private
get_weighted_values(ReadingsList, LastReading) ->
    {_, {LastHour, _, _}} = LastReading#reading.datetime,

    WeightReading = fun (R) ->
        {_, {Hour, _, _}} = R#reading.datetime,
        W = max(24 - (LastHour - Hour), 0),
        {W, R#reading.value}
        end,

    lists:map(WeightReading, ReadingsList).


get_example_data() ->
    M1 = create_monitor(),

    M2 = add_station("A", {1,1}, M1),
    M3 = add_station("B", {2,2}, M2),
    M4 = add_station("C", {3,3}, M3),

    M5 = add_value("A", {{2025, 3, 14}, {08,15,10}}, "pm10", 42, M4),
    M6 = add_value("A", {{2025, 3, 14}, {08,15,10}}, "pm2.5", 30, M5),
    M7 = add_value("A", {{2025, 3, 14}, {08,15,10}}, "temp", 18, M6),

    M8 = add_value("B", {{2025, 3, 14}, {08,10,50}}, "pm2.5", 35, M7),
    M9 = add_value("B", {{2025, 3, 14}, {08,10,50}}, "temp", 19, M8),
    M10 = add_value("B", {{2025, 3, 14}, {08,10,50}}, "hum", 18, M9),

    M11 = add_value("C", {{2025, 3, 14}, {08,45,30}}, "pm2.5", 32, M10),
    M12 = add_value("C", {{2025, 3, 14}, {08,45,30}}, "pm1", 22, M11),
    M13 = add_value("C", {{2025, 3, 14}, {08,45,30}}, "temp", 17, M12),


    M14 = add_value("A", {{2025, 3, 14}, {12,35,18}}, "pm10", 40, M13),
    M15 = add_value("A", {{2025, 3, 14}, {12,35,18}}, "pm2.5", 31, M14),
    M16 = add_value("A", {{2025, 3, 14}, {12,35,18}}, "temp", 18, M15),

    M17 = add_value("B", {{2025, 3, 14}, {12,31,43}}, "pm2.5", 38, M16),
    M18 = add_value("B", {{2025, 3, 14}, {12,31,43}}, "temp", 18, M17),
    M19 = add_value("B", {{2025, 3, 14}, {12,31,43}}, "hum", 12, M18),

    M20 = add_value("C", {{2025, 3, 14}, {12,40,05}}, "pm2.5", 29, M19),
    M21 = add_value("C", {{2025, 3, 14}, {12,40,05}}, "pm1", 23, M20),
    M22 = add_value("C", {{2025, 3, 14}, {12,40,05}}, "temp", 16, M21),


    M23 = add_value("A", {{2025, 3, 14}, {14,20,00}}, "pm10", 45, M22),
    M24 = add_value("A", {{2025, 3, 14}, {14,20,00}}, "pm2.5", 33, M23),
    M25 = add_value("A", {{2025, 3, 14}, {14,20,00}}, "temp", 19, M24),

    M26 = add_value("B", {{2025, 3, 14}, {14,18,55}}, "pm2.5", 40, M25),
    M27 = add_value("B", {{2025, 3, 14}, {14,18,55}}, "temp", 20, M26),
    M28 = add_value("B", {{2025, 3, 14}, {14,18,55}}, "hum", 15, M27),

    M29 = add_value("C", {{2025, 3, 14}, {14,35,30}}, "pm2.5", 31, M28),
    M30 = add_value("C", {{2025, 3, 14}, {14,35,30}}, "pm1", 24, M29),
    M31 = add_value("C", {{2025, 3, 14}, {14,35,30}}, "temp", 17, M30),

    M31.
