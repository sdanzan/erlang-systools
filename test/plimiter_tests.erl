-module(plimiter_tests).
-export([a_process/1]).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    Limiter = plimiter:start(10),
    ?assert(is_pid(Limiter)),
    ?assert(is_process_alive(Limiter)),
    Monitor = monitor(process, Limiter),
    plimiter:stop(Limiter),
    Dead =
        receive
            { 'DOWN', Monitor, process, Limiter, normal } -> true
        after 5000 -> false end,
    ?assert(Dead).

start_stop_registered_test() ->
    ?assert(plimiter:start(limiter, 10)),
    ?assert(is_pid(whereis(limiter))),
    Limiter = whereis(limiter),
    Monitor = monitor(process, Limiter),
    plimiter:stop(limiter),
    Dead =
        receive
            { 'DOWN', Monitor, process, Limiter, normal } -> true
        after 5000 -> false end,
    ?assert(Dead).

start_bad_args_test_() ->
    [
          { "negative max", ?_assertError(function_clause, plimiter:start(-2)) }
        , { "bad name", ?_assertError(badarg, plimiter:start("test", 10)) }
    ].

complete_flow_test_() ->
    { inorder, [
        {
            setup,
            fun() -> setup(Max) end,
            fun cleanup/1,
            [
                  { "flow_" ++ integer_to_list(Max), fun() -> flow({limiter, Max}) end }
            ]
        }
        || Max <- lists:seq(1, 1001, 200)
    ]}.

setup(Max) -> plimiter:start(limiter, Max), Max.
cleanup(_) -> plimiter:stop(limiter), unregister(limiter).

flow({Limiter, Max}) ->
    Self = self(),
    plimiter:spawn(Limiter, fun() -> a_process(Self) end),
    Pid = wait_for_a_process(),
    Pid ! { Self, stop },
    ok = wait_for_a_process_stop(Pid),
    plimiter:spawn(Limiter, plimiter_tests, a_process, [Self]),
    Pid2 = wait_for_a_process(),
    Pid2 ! { Self, stop },
    ok = wait_for_a_process_stop(Pid2),
    lists:foreach(fun(_) -> plimiter:spawn(Limiter, fun() -> a_process(Self) end) end,
                  lists:seq(1, Max)),
    [ H | Pids ] = AllPids = [ wait_for_a_process() || _ <- lists:seq(1, Max) ],
    ?assertEqual(Max, length(AllPids)),
    plimiter:spawn(Limiter, fun() -> a_process(Self) end),
    wait_for_a_process(),
    plimiter:spawn(Limiter, fun() -> a_process(Self) end),
    ?assertMatch(none, wait_for_a_process()),
    H ! { Self, stop },
    ok = wait_for_a_process_stop(H),
    L = wait_for_a_process(),
    ?assert(is_pid(L)),
    lists:foreach(fun(P) -> P ! { Self, stop } end, [ L | Pids ]),
    flush().

flush() -> receive _ -> flush() after 0 -> ok end.

wait_for_a_process() -> receive { Pid, a_process_started } -> Pid after 5 -> none end.
wait_for_a_process_stop(Pid) -> receive { Pid, a_process_stopped } -> ok end.

a_process(Parent) ->
    Parent ! { self(), a_process_started },
    receive { Parent, stop } -> Parent ! { self(), a_process_stopped } end.
