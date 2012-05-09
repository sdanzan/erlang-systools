%%% --------------------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @doc Utilities when passing parameters to os:cmd like functions.
%%% --------------------------------------------------------------------------

-module(shell_utils).
-export([quote/1]).

%% ---------------------------------------------------------------------------
-spec quote(string()) -> string().
%% @doc Replace all ' by \'
quote(String) -> lists:flatten(lists:map(fun($') -> "\\'"; (C) -> C end, String)).
