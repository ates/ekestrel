-module(ekestrel_client).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {client}).
-define(WATCHDOG_TIMER, 15000).

start_link(Options) ->
    HostName = proplists:get_value(hostname, Options),
    Port = proplists:get_value(port, Options),
    gen_server:start_link(?MODULE, [HostName, Port], []).

init([HostName, Port]) ->
    {ok, TFactory} = thrift_socket_transport:new_transport_factory(HostName, Port, [{framed, true}]),
    {ok, PFactory} = thrift_binary_protocol:new_protocol_factory(TFactory, []),
    {ok, Protocol} = PFactory(),
    {ok, Client} = thrift_client:new(Protocol, kestrel_thrift),
    erlang:send_after(?WATCHDOG_TIMER, self(), watchdog),
    {ok, #state{client = Client}}.

handle_call({set, Queue, Data, TTL}, _From, State) ->
    {Client, {ok, Reply}} =
        thrift_client:call(State#state.client, put, [Queue, Data, TTL]),
    {reply, Reply, State#state{client = Client}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(watchdog, State) ->
    {Client, {ok, _Version}} =
        thrift_client:call(State#state.client, get_version, []),
    erlang:send_after(?WATCHDOG_TIMER, self(), watchdog),
    {noreply, State#state{client = Client}};
handle_info(_Msg, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, State) ->
    {_, ok} = thrift_client:close(State#state.client), ok.
