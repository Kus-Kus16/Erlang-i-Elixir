-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_min/2,
    get_station_mean/2, get_daily_mean/2, get_moving_mean/3, crash/0]).

-define(SERVER, ?MODULE).

%%-record(pollution_gen_server_state, {}).

%%%===================================================================
%%% API
%%%===================================================================
call(Data) ->
    gen_server:call(?MODULE, Data).

cast(Data) ->
    gen_server:cast(?MODULE, Data).

add_station(Name, Coordinates) ->
    call( {add_station, Name, Coordinates} ).

add_value(StationIdentifier, Datetime, Type, Value) ->
    call( {add_value, StationIdentifier, Datetime, Type, Value} ).

remove_value(StationIdentifier, Datetime, Type) ->
    call( {remove_value, StationIdentifier, Datetime, Type} ).

get_one_value(StationIdentifier, Datetime, Type) ->
    call( {get_one_value, StationIdentifier, Datetime, Type} ).

get_station_min(StationIdentifier, Type) ->
    call( {get_station_min, StationIdentifier, Type} ).

get_station_mean(StationIdentifier, Type) ->
    call( {get_station_mean, StationIdentifier, Type} ).

get_daily_mean(Date, Type) ->
    call( {get_daily_mean, Date, Type} ).

get_moving_mean(StationIdentifier, Date, Type) ->
    call( {get_moving_mean, StationIdentifier, Date, Type} ).

crash() ->
    cast( {crash} ).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, pollution:create_monitor()}.

handle_call({get_one_value, StationIdentifier, Datetime, Type}, _From, Monitor) ->
    Result = pollution:get_one_value(StationIdentifier, Datetime, Type, Monitor),
    {reply, Result, Monitor};

handle_call({get_station_min, StationIdentifier, Type}, _From, Monitor) ->
    Result = pollution:get_station_min(StationIdentifier, Type, Monitor),
    {reply, Result, Monitor};

handle_call({get_station_mean, StationIdentifier, Type}, _From, Monitor) ->
    Result = pollution:get_station_mean(StationIdentifier, Type, Monitor),
    {reply, Result, Monitor};

handle_call({get_daily_mean, Date, Type}, _From, Monitor) ->
    Result = pollution:get_daily_mean(Date, Type, Monitor),
    {reply, Result, Monitor};

handle_call({get_moving_mean, StationIdentifier, Date, Type}, _From, Monitor) ->
    Result = pollution:get_moving_mean(StationIdentifier, Date, Type, Monitor),
    {reply, Result, Monitor};

handle_call({add_station, Name, Coordinates}, _From, Monitor) ->
    case pollution:add_station(Name, Coordinates, Monitor) of
        Error = {error, _} ->
            {reply, Error, Monitor};
        NewMonitor ->
            {reply, ok, NewMonitor}
    end;

handle_call({add_value, StationIdentifier, Datetime, Type, Value}, _From, Monitor) ->
    case pollution:add_value(StationIdentifier, Datetime, Type, Value, Monitor) of
        Error = {error, _} ->
            {reply, Error, Monitor};
        NewMonitor ->
            {reply, ok, NewMonitor}
    end;

handle_call({remove_value, StationIdentifier, Datetime, Type}, _From, Monitor) ->
    case pollution:remove_value(StationIdentifier, Datetime, Type, Monitor) of
        Error = {error, _} ->
            {reply, Error, Monitor};
        NewMonitor ->
            {reply, ok, NewMonitor}
    end.

handle_cast({crash}, _Monitor) ->
    no:exist().

terminate(Reason, Monitor) ->
    io:format("Server: exit with value ~w~n", [Monitor]),
    Reason.

%%handle_info(_Info, State = #pollution_gen_server_state{}) ->
%%    {noreply, State}.
%%
%%code_change(_OldVsn, State = #pollution_gen_server_state{}, _Extra) ->
%%    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
