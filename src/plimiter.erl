%%% --------------------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @doc A process limiter.
%%% @end
%%% --------------------------------------------------------------------------

-module(plimiter).
-export([start/1, start/2, start_link/1, start_link/2, stop/1]).
-export([spawn/2, spawn/4]).
-export([spawn_link/2, spawn_link/4]).

-type limiter() :: pid() | atom().
-export_type([limiter/0]).

%% ---------------------------------------------------------------------------
-spec start(Max :: pos_integer()) -> Limiter :: limiter().
%% @doc Start a plain process limiter.

start(Max) -> starter(Max, fun spawn/1).

%% ---------------------------------------------------------------------------
-spec start(Name :: atom(), Max :: pos_integer()) -> true.
%% @doc Start a plain process limiter with a given name.

start(Name, Max) -> register(Name, start(Max)).

%% ---------------------------------------------------------------------------
-spec start_link(Max :: pos_integer()) -> Limiter :: limiter().
%% @doc Start a plain process limiter linked to the creator.

start_link(Max) -> starter(Max, fun spawn_link/1).

%% ---------------------------------------------------------------------------
-spec start_link(Name :: atom(), Max :: pos_integer()) -> true.
%% @doc Start a plain process named limiter linked to the creator.

start_link(Name, Max) -> register(Name, start_link(Max)).

%% ---------------------------------------------------------------------------
-spec stop(Limiter :: limiter()) -> ok.
%% @doc Stop a process limiter.
%%      All queued messages at the time of call will be processed first.
%%      Only the limiter creator can stop the limiter.
%% @end

stop(Limiter) when is_pid(Limiter) -> Limiter ! { self(), stop }, ok;
stop(Limiter) -> stop(whereis(Limiter)).

%% ---------------------------------------------------------------------------
-spec spawn(Limiter :: limiter(), Fun :: fun(() -> any())) -> ok.
%% @doc Spawn a process through a limiter.
spawn(Limiter, Fun) when is_pid(Limiter) -> Limiter ! { spawn, Fun }, ok;
spawn(Limiter, Fun) -> plimiter:spawn(whereis(Limiter), Fun).

%% ---------------------------------------------------------------------------
-spec spawn(Limiter :: limiter(), 
            Module :: atom(), Function :: fun(), Args :: list()) -> ok.
%% @doc Spawn a process through a limiter.
spawn(Limiter, Module, Function, Args) when is_pid(Limiter) ->
    Limiter ! { spawn, Module, Function, Args }, ok;
spawn(Limiter, Module, Function, Args) ->
    plimiter:spawn(whereis(Limiter), Module, Function, Args).

%% ---------------------------------------------------------------------------
-spec spawn_link(Limiter :: limiter(), Fun :: fun(() -> any())) -> ok.
%% @doc Spawn a process through a limiter. When effectively started
%%      the process will be linked to caller.
%% @end
spawn_link(Limiter, Fun) when is_pid(Limiter) ->
    Limiter ! { spawn_link, Fun, self() };
spawn_link(Limiter, Fun) -> plimiter:spawn_link(whereis(Limiter), Fun).

%% ---------------------------------------------------------------------------
-spec spawn_link(Limiter :: limiter(), 
                 Module :: atom(), Function :: fun(), Args :: list()) -> ok.
%% @doc Spawn a process through a limiter. When effectively started
%%      the process will be linked to caller.
%% @end
spawn_link(Limiter, Module, Function, Args) when is_pid(Limiter)->
    Limiter ! { spawn_link, Module, Function, Args, self() };
spawn_link(Limiter, Module, Function, Args) ->
    plimiter:spawn_link(whereis(Limiter), Module, Function, Args).

%% ---------------------------------------------------------------------------

%% ---------------------------------------------------------------------------
starter(Max, StarterFun) when Max > 0 ->
    Self = self(),
    StarterFun(fun() -> limiter_loop(Self, Max, 0) end).

%% ---------------------------------------------------------------------------
%% The main loop for the limiter process:
%%      - child processes are monitored
%%      - when max concurrent processes is reached, we wait for a process to
%%        end
limiter_loop(Creator, Max, Current) when Current >= Max ->
    receive
        { 'DOWN', _, process, _, _ } -> limiter_loop(Creator, Max, Current - 1)
    end;
limiter_loop(Creator, Max, Current) ->
    receive
        { 'DOWN', _, process, _, _ } ->
            limiter_loop(Creator, Max, Current - 1);
        
        Any when Any =/= { Creator, stop } ->
            case Any of
                { spawn, Fun }
                    -> spawn_monitor(Fun);

                { spawn, Module, Function, Args } 
                    -> spawn_monitor(Module, Function, Args);

                { spawn_link, Fun, Pid }
                    -> spawn_monitor(fun() -> link(Pid), Fun() end);

                { spawn_link, Module, Function, Args, Pid }
                    -> spawn_monitor(fun() -> link(Pid), apply(Module, Function, Args) end)
            end,
            limiter_loop(Creator, Max, Current + 1);

        { Creator, stop } -> ok
    end.
