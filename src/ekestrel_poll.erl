-module(ekestrel_poll).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {queue, client}).

-define(POLL_TIME, 2000).
-define(MAX_ITEMS, 10).

start_link(Name, Queue, Options) ->
    HostName = proplists:get_value(hostname, Options),
    Port = proplists:get_value(port, Options),
    gen_server:start_link({local, Name}, ?MODULE, [Queue, HostName, Port], []).

init([Queue, HostName, Port]) ->
    {ok, TFactory} = thrift_socket_transport:new_transport_factory(HostName, Port, [{framed, true}]),
    {ok, PFactory} = thrift_binary_protocol:new_protocol_factory(TFactory, []),
    {ok, Protocol} = PFactory(),
    {ok, Client} = thrift_client:new(Protocol, kestrel_thrift),
    pg2:create(Queue),
    erlang:send_after(?POLL_TIME, self(), check_queue),
    {ok, #state{queue = Queue, client = Client}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(check_queue, #state{client = Client, queue = Queue} = State) ->
    {NewClient, {ok, Items}} =
        thrift_client:call(Client, get, [Queue, ?MAX_ITEMS, ?POLL_TIME, 0]),
    case length(Items) of
        0 ->
            erlang:send_after(?POLL_TIME, self(), check_queue);
        _ ->
            [Pid ! {kestrel_msg, Items} || Pid <- pg2:get_local_members(Queue)],
            erlang:send_after(0, self(), check_queue)
    end,
    {noreply, State#state{client = NewClient}};
handle_info(_Msg, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, State) ->
    {_, ok} = thrift_client:close(State#state.client), ok.
