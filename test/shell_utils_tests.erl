-module(shell_utils_tests).

-include_lib("eunit/include/eunit.hrl").

quote_test_() ->
    [
          { "quote / single", ?_assertEqual("\\'", shell_utils:quote("'")) }
        , { "quote / none",    ?_assertEqual("x", shell_utils:quote("x")) }
        , { "quote / multiple", 
            ?_assertEqual("abcdef\\'h\\'jr", shell_utils:quote("abcdef'h'jr")) }
        , { "quote / empty", ?_assertEqual("", shell_utils:quote("")) }
    ].
