%%%-------------------------------------------------------------------
%% @doc makota top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(makota_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 1
    },
    ChildSpecs = [
        #{  id => pgs,
            start => {pollution_gen_server, start_link, []} },
        #{  id => pvcgs,
            start => {pollution_value_collector_gen_statem, start_link, []} }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
