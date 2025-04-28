%%%-------------------------------------------------------------------
%% @doc makota public API
%% @end
%%%-------------------------------------------------------------------

-module(makota_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    makota_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
