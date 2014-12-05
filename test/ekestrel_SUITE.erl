-module(ekestrel_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% tests
-export([subscribe_and_set/1]).

all() -> [subscribe_and_set].

init_per_suite(Config) ->
    Pool =  [
        {k1, [
            {size, 5},
            {max_overflow, 3}
        ], [
            {hostname, "127.0.0.1"},
            {port, 2229},
            {poll_time, 2000},
            {max_items, 10},
            {timeout, 2000}
        ]}
    ],
    application:set_env(ekestrel, pools, Pool),
    ekestrel:start(),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% tests
subscribe_and_set(_Config) ->
    ok = ekestrel:subscribe("test_queue"),
    1 = ekestrel:set("test_queue", <<"test_message">>, 5000),
    receive
        {kestrel_msg, _Pid, "test_queue", [{item, <<"test_message">>, 0}]} = Msg ->
            ct:log("Got message: ~p", [Msg])
    after 5000 ->
        throw(enoent)
    end.
