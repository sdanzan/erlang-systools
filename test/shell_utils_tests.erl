-module(shell_utils_tests).

-include_lib("eunit/include/eunit.hrl").

quote_test_() ->
    [
          ?_assertEqual("\\'", shell_utils:quote("'"))
        , ?_assertEqual("x", shell_utils:quote("x"))
        , ?_assertEqual("abcdef\\'h\\'jr", shell_utils:quote("abcdef'h'jr"))
        , ?_assertEqual("", shell_utils:quote(""))
    ].
