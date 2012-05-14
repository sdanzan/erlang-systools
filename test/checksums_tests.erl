-module(checksums_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_FILE,   "../test/data.txt").
-define(TEST_MD5,    "388740922dbebd0ce836545263ce5e5a").
-define(TEST_SHA1,   "601bb735a5c1ddae72babc5ef1d14f3152e5fd02").
%-define(TEST_SHA224, "8a19267ff8b6c4a0d46ce494dbb3b39de5ad79ab03fa5b7683af783e"). 
-define(TEST_SHA256, "63877de40f3967fa63b575dd42c1292890c94324fedc4194d312452ea5c5754a").
%-define(TEST_SHA384, "8179cfbe13ad8421a10854672be6db3e46f928cd6b77df6d6575f4f6f6b662b46b2be748dd1a2d9954f2783ae680db93").
-define(TEST_SHA512, "5608ec315fa5e00880564bddbc79ff5ee4760b1c4f1c7650188130a575fffa2d2e5354e7020cee3276571657a15f901bfffedca9be4d62dd41eb22b5a9d3f2ef").

checksums_test_() ->
    [
          { "md5",    ?_assertEqual(?TEST_MD5,    checksums:md5sum(?TEST_FILE)) }
        , { "md5",    ?_assertEqual(?TEST_MD5,    checksums:md5sum(?TEST_FILE, fast)) }
        , { "md5",    ?_assertEqual(begin D = list_to_integer(?TEST_MD5, 16), <<D:128>> end,
                                    checksums:md5sum(?TEST_FILE, binary)) }
        , { "sha1",   ?_assertEqual(?TEST_SHA1,   checksums:sha1sum(?TEST_FILE)) }
        , { "sha1",   ?_assertEqual(?TEST_SHA1,   checksums:sha1sum(?TEST_FILE, fast)) }
        , { "sha1",   ?_assertEqual(begin D = list_to_integer(?TEST_SHA1, 16), <<D:160>> end,
                                    checksums:sha1sum(?TEST_FILE, binary)) }
%        , { "sha224", ?_assertEqual(?TEST_SHA224, checksums:sha224sum(?TEST_FILE)) }
        , { "sha256", ?_assertEqual(?TEST_SHA256, checksums:sha256sum(?TEST_FILE)) }
%        , { "sha256", ?_assertEqual(?TEST_SHA256, checksums:sha256sum(?TEST_FILE, fast)) }
        , { "sha256", ?_assertEqual(begin D = list_to_integer(?TEST_SHA256, 16), <<D:256>> end,
                                    checksums:sha256sum(?TEST_FILE, binary)) }
%        , { "sha384", ?_assertEqual(?TEST_SHA384, checksums:sha384sum(?TEST_FILE)) }
        , { "sha512", ?_assertEqual(?TEST_SHA512, checksums:sha512sum(?TEST_FILE)) }
%        , { "sha512", ?_assertEqual(?TEST_SHA512, checksums:sha512sum(?TEST_FILE, fast)) }
        , { "sha512", ?_assertEqual(begin D = list_to_integer(?TEST_SHA512, 16), <<D:512>> end,
                                    checksums:sha512sum(?TEST_FILE, binary)) }
    ].
