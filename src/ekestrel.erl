-module(ekestrel).

%% API
-export([start/0]).
-export([set/3, subscribe/1, subscribe/2, unsubscribe/1]).

start() ->
    {ok, _Started} = application:ensure_all_started(?MODULE).

-spec set(string(), binary(), non_neg_integer()) -> non_neg_integer().
set(Queue, Data, TTL) when is_list(Queue), is_binary(Data) ->
    F = fun(W) -> gen_server:call(W, {set, Queue, Data, TTL}) end,
    poolboy:transaction(active_pool(), F).

-spec subscribe(string()) -> ok.
subscribe(Queue) when is_list(Queue) ->
    subscribe(Queue, []).

subscribe(Queue, Options) when is_list(Queue) ->
    {ok, Values} = application:get_env(ekestrel, pools),
    Pools = [{Name, estd_lists:merge(Opts, Options)} || {Name, _, Opts} <- Values],
    do_subscribe(Pools, Queue),
    pg2:join(Queue, self()).

do_subscribe([], _Queue) -> ok;
do_subscribe([{Pool, Opts} | Tail], Queue) ->
    Name = list_to_atom(string:join([atom_to_list(Pool), Queue], "_")),
    Spec = {
        Name,
        {ekestrel_poll, start_link, [Name, Queue, Opts]},
        permanent, 5000, worker, [ekestrel_poll]
    },
    supervisor:start_child(ekestrel_poll_sup, Spec),
    do_subscribe(Tail, Queue).

-spec unsubscribe(string()) -> ok.
unsubscribe(Queue) ->
    pg2:leave(Queue, self()),
    case pg2:get_local_members(Queue) of
        [] ->
            {ok, Values} = application:get_env(ekestrel, pools),
            Pools = [Name || {Name, _, _} <- Values],
            F = fun(N) ->
                Name = list_to_atom(string:join([atom_to_list(N), Queue], "_")),
                supervisor:terminate_child(ekestrel_poll_sup, Name),
                supervisor:delete_child(ekestrel_poll_sup, Name)
            end,
            lists:foreach(F, Pools);
        _ -> ok
    end.

active_pool() ->
    Pools = [Name || {Name, _, _, _} <- supervisor:which_children(ekestrel_pools_sup)],
    lists:nth(random:uniform(length(Pools)), Pools).
