%%% --------------------------------------------------------------------------
%%% @doc Dummy application module for the systool library.
%%% --------------------------------------------------------------------------

-module(systools_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    systools_sup:start_link().

stop(_State) ->
    ok.
