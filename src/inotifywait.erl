%%%---------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @version 1.0.0
%%% @doc A wrapper around inotify using a simple port
%%%      around the 'inotifywait' executable.
%%% @end
%%%---------------------------------------------------------------

%%% License: LGPLv3
%%% See: http://www.gnu.org/copyleft/lesser.html

-module(inotifywait).
-author("Serge Danzanvilliers <serge.danzanvilliers@gmail.com>").
-export([start/2, start/1,
         start_event_manager/2, start_event_manager/3,
         close/1]).

%% Inotifywait event record
-record(inotify, { event, isdir, file, watched }).

%% ---------------------------------------------------------------------------
-spec start(Monitored :: string(), Options :: list()) -> pid().
%% @doc Start an inotifywait wrapper process.
%%
%%      Monitored: monitored file/directory.
%%      Options is a list of tagged tuples, allowed
%%      tags are: (see the inotifywait manual for more information)
%%              recursive: also watches all subdirectories.
%%              {exclude, Regexp}: exclude files matching Regexp.
%%              {excludei, Regexp}: exclude files matching case insensitive Regexp.
%%              {events, [ events ]}: monitored events.
%% @end
start(Monitored, Options) ->
    To = self(),
    spawn_link(fun() -> start_inotify_loop(Monitored, Options, To) end).

%% ---------------------------------------------------------------------------
-spec start(Monitored :: string()) -> pid().
%% @equiv start(Monitored, [])
start(Monitored) -> start(Monitored, []).

%% ---------------------------------------------------------------------------
-spec start_event_manager(Name::atom(), Monitored::string(), Options::list())
        -> ok.
%% @doc Starts an event manager associated to an inotifywait wrapper process.
%%      That allows use of the gen_vent behaviours to subscribe to inotifywait
%%      generated events. The inotify wrapper is started automatically.
%% 
%%      Name: the name of the event manager.
%% @end
%% @see start/2. Monitored and Options are the same as in start.
start_event_manager(Name, Monitored, Options) ->
    { ok, _ } = gen_event:start_link(Name),
    spawn_link(fun() -> event_manager_loop(Name, Monitored, Options) end),
    ok. 

%% ---------------------------------------------------------------------------
-spec start_event_manager(Name::atom(), Monitored::string()) -> ok.
%% @equiv start_event_manager(Name, Monitored, [])
start_event_manager(Name, Monitored) -> start_event_manager(Name, Monitored, []).

%% ---------------------------------------------------------------------------
-spec close(InotifyWrapper :: pid()) -> ok | not_ok.
%% @doc Close an inotify wrapper process.
%% Should return ok if nothing went wrong. Will return not_ok
%% if closing is not acknowledged after 1 second.
%% @end
close(InotifyWrapper) ->
    InotifyWrapper ! { self(), close },
    receive
        { InotifyWrapper, closed } -> ok
    after 1000 -> not_ok end.

%% ---------------------------------------------------------------------------
%% Private functions

%% ---------------------------------------------------------------------------
%% Start the inotify loop: create the port and loop on messages.
start_inotify_loop(Monitored, Options, To) ->
    InotifyPort = inotify_port(Monitored, Options),
    SPid = % first message sent is the external process os pid.
        receive
            { InotifyPort, { data, { eol, Line } } } -> Line
        after 1000 -> % timeout in case something goes wrong
            error_logger:error_msg("Unable to communicate with inotifywait."),
            erlang:error(inotify, [ Monitored, Options, To ])
        end,
    error_logger:info_msg("Starting inotify wrapper [~w / ~w (pid: ~s)]~n", 
                          [ self(), InotifyPort, SPid ]),
    inotify_loop(To, InotifyPort, "kill " ++ SPid).

%% ---------------------------------------------------------------------------
%% Main loop - listen from an inotify port and send back messages to creator.
inotify_loop(To, InotifyPort, KillCmd) ->
    process_flag(trap_exit, true), % trap exit to be able to cleanup
    inotify_loop(To, InotifyPort, KillCmd, "").
inotify_loop(To, InotifyPort, KillCmd, PartialLine) ->
    receive
        % EXIT signal - cleanup and exit
        { 'EXIT', _, _ } -> port_close(InotifyPort), os:cmd(KillCmd);

        % close the port, kill external process, send back status and exit
        { To, close } ->
            error_logger:info_msg("Closing inotify port [~w / ~w]~n", [ self(), InotifyPort ]),
            port_close(InotifyPort),
            error_logger:info_msg("Killing external process: '~s'~n", [ KillCmd ]),
            os:cmd(KillCmd),
            To ! { self(), closed };

        % Inotify messages - partial lines - should not happen since
        % inotifywait output length seems to be limited to around 4000
        % and we use a 16384 char long buffer.
        { InotifyPort, { data, { noeol, Line } } } ->
            inotify_loop(To, InotifyPort, KillCmd, PartialLine ++ Line);

        % Inotify messages - parse and forward to 'To'
        { InotifyPort, { data, { eol, Line } } } ->
            To ! { self(), parse_inotify_event(PartialLine ++ Line) },
            inotify_loop(To, InotifyPort, KillCmd, "");

        % External process end
        { InotifyPort, { exit_status, Status } } ->
            error_logger:error_msg("~w: inotifwait ended unexpectedly with status ~w~n",
                                   [ InotifyPort, Status ]),
            port_close(InotifyPort),
            To ! { self(), closed_unexpectedly }
    end.

%% ---------------------------------------------------------------------------
%% Parse a line sent by an inotifywait process and turn it to an event message.
%% -record(inotify, { event, isdir, file, watched })
parse_inotify_event(Line) ->
    [ Watched, Events, File ] =
        case string:tokens(Line, "|") of
            [ W, E ]          -> [ W, E, W ];
            [ _, _, _ ] = WEF -> WEF
        end,
    EToks = [ list_to_atom(Tok) || Tok <- string:tokens(string:to_lower(Events), ",") ], 
    { Event, IsDir } =
        case EToks of
            [ Any ]               -> { Any, false };
            [ Any, isdir ]        -> { Any, true };
            [ Any, close ]        -> { Any, false };
            [ Any, close, isdir ] -> { Any, true }
        end,
    #inotify{ event = Event, isdir = IsDir, file = File, watched = Watched }.

%% ---------------------------------------------------------------------------
%% gen_event notifyer loop.
event_manager_loop(Manager, Monitored, Options) ->
    Wrapper = start(Monitored, Options),
    try event_manager_loop(Wrapper, Manager)
    after
        close(Wrapper)
    end.
event_manager_loop(Wrapper, Manager) ->
    receive
        { Wrapper, Data } -> gen_event:notify(Manager, Data)
    end,
    event_manager_loop(Wrapper, Manager).

%% ---------------------------------------------------------------------------
%% Parse options to pass to inotifywait, bad options will trigger an error.
parse_options(Options) ->
    try parse_options("-m -q", Options)
    catch throw:bad_option -> erlang:error(badarg, Options) end.
parse_options(Args, []) -> Args;
parse_options(Args, [ Option | Tail ]) ->
    case Option of
        recursive ->
            parse_options(Args ++ " -r ", Tail);
        { events, List } ->
            parse_options(Args ++ " -e " ++ parse_options_events(List), Tail);
        { exclude, Pattern } ->
            parse_options(Args ++ " --exclude " ++ Pattern, Tail);
        { excludei, Pattern } -> 
            parse_options(Args ++ " --excludei " ++ Pattern, Tail);

        Any -> error_logger:error_msg("Unsupported option: ~w~n", [ Any ]),
               throw(bad_option)
    end.

parse_options_events(Events) -> 
    Allowed = [ access, modify, attrib, close_write, close_nowrite, close, open,
                moved_to, moved_from, move_self, create, delete, delete_self,
                unmount ],
    SEvents = lists:usort(Events),
    case lists:filter(fun(E) -> lists:member(E, Allowed) end, SEvents) of
        [] -> string:join([ atom_to_list(E) || E <- SEvents ], ",");
        List ->
            error_logger:error_msg("Inotify events not supported: ~w~n", [ List ]),
            erlang:error(badarg, Events)
    end.

%% ---------------------------------------------------------------------------
%% Launch an external inotifywait process through a port.
%% Some shell trick is used to get the real (os) pid to be able to end the
%% external process when need be.
inotify_port(Monitored, Options) ->
    % Arguments to inotifywait
    Args = parse_options(Options) ++ " --format \"%w|%e|%f\" " ++ Monitored,

    % Trick to return the external os pid:
    % launch through 'sh -c' to echo the pid then exec to inotifywait.
    Command = "sh -c 'echo $$; exec $0 $*' inotifywait " ++ Args,

    Port = open_port({ spawn, Command },
                     [ { line, 16384 }, stderr_to_stdout, eof ]),
    error_logger:info_msg(
        "Starting inotify port ~w with command: 'inotifywait ~s'~n",
        [ Port, Args ]),
    Port.
