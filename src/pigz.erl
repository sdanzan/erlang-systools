%%% -------------------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @doc A wrapper around the pigz program. Provide a "compressor" process
%%%      to send file compresion orders.
%%% @end
%%% -------------------------------------------------------------------------

-module(pigz).
-export([create_compressor/0, create_compressor/1, delete_compressor/1]).
-export([compress/2, start_compress/2]).

%% --------------------------------------------------------------------------
-spec create_compressor(list()) -> pid().
%% @doc Create a compressor process to wait for compression tasks.
%%      Allowed options:
%%              { level, Level }: compression level (default 9).
%%              { parallel, Parallel }: compression threads.
%%              silent: no logging.
%% @end
create_compressor(Options) ->
    Cmd = "pigz " 
          ++ check_level(proplists:get_value(level, Options, 9))
          ++ check_parallel(proplists:get_value(parallel, Options)),
    process_compression(Cmd, proplists:get_value(silent, Options, false)).

%% --------------------------------------------------------------------------
-spec create_compressor() -> pid().
%% @equiv create_compressor([])
create_compressor() -> create_compressor([]).

check_level(Level) when Level >= 0 orelse Level =< 9 ->
    io_lib:format(" -~w ", [ Level ]).

check_parallel(undefined) -> "";
check_parallel(Parallel) when Parallel > 0 ->
    io_lib:format(" -p ~w ", [ Parallel ]).

%% --------------------------------------------------------------------------
-spec delete_compressor(pid()) -> ok.
%% @doc Requires the given compressor to stop serving compression tasks.
delete_compressor(Compressor) -> Compressor ! { self(), stop }, ok.

%% --------------------------------------------------------------------------
-spec compress(String :: string(), Compressor :: pid()) -> string().
%% @doc Synchronously compress a given file and return the compressed file name.
compress(FileName, Compressor) ->
    start_compress(FileName, Compressor),
    receive
        { Compressor, { success, CompressedFileName } } -> CompressedFileName
    end.

%% --------------------------------------------------------------------------
-spec start_compress(String :: string(), Compressor :: pid()) -> string().
%% @doc Send a compression request to a compressor. Once finished the
%%      compressor will send back a message:
%%         { Compressor, { success, CompressedFileName } }
%%      or { Compressor, { error, Reason } }
%% @end
start_compress(FileName, Compressor) -> Compressor ! { self(), FileName }, ok.

%% --------------------------------------------------------------------------

%% --------------------------------------------------------------------------
%% Wait for compression loop
process_compression(Cmd, Silent) ->
    receive
        { _, stop } -> ok;
        { From, FileName } ->
            Command = Cmd ++ shell_utils:quote(FileName),
            if Silent =:= false ->
                error_logger:info_msg("Starting compression: ~s~n", [ Command ]);
               true -> ok
            end,
            case os:cmd(Command ++ "; echo $?") of
                "0" -> From ! { self(), { success, FileName ++ ".gz" } };
                Reason -> From ! { self(), { error, Reason } }
            end,
            process_compression(Cmd, Silent)
    end.

