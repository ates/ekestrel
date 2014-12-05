-module(ekestrel_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SPEC(M), {M, {M, start_link, []}, permanent, infinity, supervisor, [M]}).
-define(CHILDS, [ekestrel_pools_sup, ekestrel_poll_sup]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?SPEC(M) || M <- ?CHILDS]}}.
