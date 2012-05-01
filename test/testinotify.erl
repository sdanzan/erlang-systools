%%% -------------------------------------------------------------------------
%%% Sample test / demonstration program.
%%% Simply call testinotify:test() in erlang shell and play around with the
%%% watched directory / file. To stop the program, just touch/create a file
%%% named 'stop' in the watched directory.
%%%
%%% ex.:
%%%     test(".").
%%%     test("/tmp", [ recursive ]).
%%%     test("/tmp", [ { exclude, "toto" }, { events, [ close, attrib ] } ]).

-module(testinotify).
-export([test/0, test/1, test/2]).

-record(inotify, { event, isdir, file, watched }).

test() -> test(".").
test(Watched) -> test(Watched, []).
test(Watched, Options) ->
    Wrapper = inotifywrapper:start(Watched, Options),
    loop(Wrapper).

loop(Wrapper) ->
    receive
        { Wrapper, Event } ->
            case Event#inotify.event of
                access -> io:format("~s was accessed (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                attrib -> io:format("~s metadata was nodified (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                modify -> io:format("~s was modified (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                close_write -> io:format("~s was closed (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                close_nowrite  -> io:format("~s was closed read only (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                open   -> io:format("~s was opened (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                moved_to -> io:format("~s was moved to (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                moved_from -> io:format("~s was moved from (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                create -> io:format("~s was created (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ]);
                delete -> io:format("~s was deleted (dir: ~w).~n",
                                    [ Event#inotify.file, Event#inotify.isdir ])
            end,
            case Event#inotify.file of
                "stop" -> inotifywrapper:close(Wrapper);
                _ -> loop(Wrapper)
            end
    after 120000 ->
        inotifywrapper:close(Wrapper), nok
    end.
