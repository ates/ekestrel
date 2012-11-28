-module(ekestrel_pools_sup).

-behaviour(supervisor2).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Pools = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, ekestrel_client}] ++ SizeArgs,
        {Name, {poolboy, start_link, [PoolArgs, WorkerArgs]},
            {permanent, 10}, 5000, worker, [poolboy]}
%        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, pools()),
    {ok, {{one_for_one, 5, 5}, Pools}}.

%% Internal functions
pools() ->
    case application:get_env(ekestrel, pools) of
        {ok, Value} ->
            Value;
        _ -> []
    end.
