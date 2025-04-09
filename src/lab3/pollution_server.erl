-module(pollution_server).

-export([start/0, init/0, stop/0, add_station/2, add_value/4, remove_value/3,
    get_one_value/3, get_station_min/2, get_station_mean/2, get_daily_mean/2,
    get_moving_mean/3]).

start() ->
    register(server, spawn(?MODULE, init, [])).

init() ->
    loop(pollution:create_monitor()).

loop(Monitor) ->
    receive
        {Pid, stop, _} ->
            Pid ! {reply, ok};

        {Pid, add_station, {Name, Coordinates}} ->
            case pollution:add_station(Name, Coordinates, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {reply, ok},
                    loop(NewMonitor)
            end;

        {Pid, add_value, {StationIdentifier, Datetime, Type, Value}} ->
            case pollution:add_value(StationIdentifier, Datetime, Type, Value, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {reply, ok},
                    loop(NewMonitor)
            end;

        {Pid, remove_value, {StationIdentifier, Datetime, Type}} ->
            case pollution:remove_value(StationIdentifier, Datetime, Type, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                NewMonitor ->
                    Pid ! {reply, ok},
                    loop(NewMonitor)
            end;

        {Pid, get_one_value, {StationIdentifier, Datetime, Type}} ->
            case pollution:get_one_value(StationIdentifier, Datetime, Type, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                Value ->
                    Pid ! {reply, Value},
                    loop(Monitor)
            end;

        {Pid, get_station_min, {StationIdentifier, Type}} ->
            case pollution:get_station_min(StationIdentifier, Type, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                Value ->
                    Pid ! {reply, Value},
                    loop(Monitor)
            end;

        {Pid, get_station_mean, {StationIdentifier, Type}} ->
            case pollution:get_station_mean(StationIdentifier, Type, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                Value ->
                    Pid ! {reply, Value},
                    loop(Monitor)
            end;

        {Pid, get_daily_mean, {Date, Type}} ->
            case pollution:get_daily_mean(Date, Type, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                Value ->
                    Pid ! {reply, Value},
                    loop(Monitor)
            end;

        {Pid, get_moving_mean, {StationIdentifier, Date, Type}} ->
            case pollution:get_moving_mean(StationIdentifier, Date, Type, Monitor) of
                {error, _} = Error ->
                    Pid ! {reply, Error},
                    loop(Monitor);
                Value ->
                    Pid ! {reply, Value},
                    loop(Monitor)
            end
    end.

%% client:
request(Request, Data) ->
    server ! {self(), Request, Data},
    receive
        {reply, Reply} -> Reply
    end.

stop() ->
    request(stop, {}).

add_station(Name, Coordinates) ->
    request(add_station, {Name, Coordinates}).

add_value(StationIdentifier, Datetime, Type, Value) ->
    request(add_value, {StationIdentifier, Datetime, Type, Value}).

remove_value(StationIdentifier, Datetime, Type) ->
    request(remove_value, {StationIdentifier, Datetime, Type}).

get_one_value(StationIdentifier, Datetime, Type) ->
    request(get_one_value, {StationIdentifier, Datetime, Type}).

get_station_min(StationIdentifier, Type) ->
    request(get_station_min, {StationIdentifier, Type}).

get_station_mean(StationIdentifier, Type) ->
    request(get_station_mean, {StationIdentifier, Type}).

get_daily_mean(Date, Type) ->
    request(get_daily_mean, {Date, Type}).

get_moving_mean(StationIdentifier, Date, Type) ->
    request(get_moving_mean, {StationIdentifier, Date, Type}).