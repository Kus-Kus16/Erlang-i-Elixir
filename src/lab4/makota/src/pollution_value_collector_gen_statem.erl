
-module(pollution_value_collector_gen_statem).
-author("Maciej").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).
-export([set_station/1, add_value/3, store_data/0]).
-export([no_station/3, collecting_values/3]).

-define(SERVER, ?MODULE).

-record(state, {stationidentifier = none, values = []}).


start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, no_station, #state{}}.

%%%===================================================================
%%% API
%%%===================================================================

set_station(StationIdentifier) ->
    gen_statem:cast(?SERVER, {set_station, StationIdentifier}).

add_value(Datetime, Type, Value) ->
    gen_statem:cast(?SERVER, {add_value, Datetime, Type, Value}).

store_data() ->
    gen_statem:cast(?SERVER, {store_data}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    state_functions.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
no_station(_Event, {set_station, StationIdentifier}, _State) ->
    {next_state, collecting_values, #state{ stationidentifier = StationIdentifier }}.

collecting_values(_Event, {add_value, Datetime, Type, Value}, State) ->
    Values = State#state.values ++ [{Datetime, Type, Value}],
    {next_state, collecting_values, State#state{ values = Values }};

collecting_values(_Event, {store_data}, State) ->
    StationIdentifier = State#state.stationidentifier,
    [ pollution_gen_server:add_value(StationIdentifier, Datetime, Type, Value) ||
        {Datetime, Type, Value} <- State#state.values ],
    {next_state, no_station, #state{}}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State) ->
    ok.