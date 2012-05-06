%%% --------------------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @doc Wrapper over the md5sum, sha1sum, sha224sum, sha256, sha384sum and
%%%      sha512sum utilities.
%%% @end
%%% --------------------------------------------------------------------------

-module(checksums).
-export([md5sum/1, sha1sum/1, sha224sum/1, sha256sum/1, sha384sum/1, sha512sum/1]).
-export([emd5sum/1, emd5sum/2]).

-define(DEFAULT_BUFFER_LENGTH, 65536).

%%% --------------------------------------------------------------------------
%%% @doc MD5 digest through md5sum utility
md5sum(FileName) -> gensum(FileName, "md5sum").

%%% --------------------------------------------------------------------------
%%% @doc SHA1 digest through sha1sum utility
sha1sum(FileName) -> gensum(FileName, "sha1sum").

%%% --------------------------------------------------------------------------
%%% @doc SHA224 digest through sha224sum utility
sha224sum(FileName) -> gensum(FileName, "sha224sum").

%%% --------------------------------------------------------------------------
%%% @doc SHA256 digest through sha256sum utility
sha256sum(FileName) -> gensum(FileName, "sha256sum").

%%% --------------------------------------------------------------------------
%%% @doc SHA384 digest through sha384sum utility
sha384sum(FileName) -> gensum(FileName, "sha384sum").

%%% --------------------------------------------------------------------------
%%% @doc SHA1 digest through sha512sum utility
sha512sum(FileName) -> gensum(FileName, "sha512sum").

%%% --------------------------------------------------------------------------
%%% Launch a xxxsum utility and get back the result.
gensum(FileName, Cmd) ->
    FullCmd = Cmd ++ " " ++ shell_utils:quote(FileName) ++ "; echo$?",
    case sring:tokens(os:cmd(FullCmd), "\n") of
        [ Output, "0" ] -> hd(string:tokens(Output, " "));
        [ Error | _ ]   -> { error, Error }
    end.

%%% --------------------------------------------------------------------------
%%% @doc Compute a MD5 checksum of the given file using plain erlang.
emd5sum(FileName) -> emd5sum(FileName, ?DEFAULT_BUFFER_LENGTH).

%%% --------------------------------------------------------------------------
%%% @doc Compute a MD5 checksum of the given file using plain erlang.
%%%      Use a given length for the read buffer.
%%% @end
emd5sum(FileName, BufferLength) ->
    { ok, File } = file:open(FileName, [ read, binary, raw ]),
    try md5sum_loop(File, BufferLength, crypto:md5_init())
    after file:close(File) end.

%%% --------------------------------------------------------------------------
%%% MD5 checksum computation loop.
md5sum_loop(File, BufferLength, Md5Context) ->
    case file:read(File, BufferLength) of
        { ok, Data } -> md5sum_loop(File,
                                    BufferLength,
                                    crypto:md5_update(Md5Context, Data));
        eof -> { ok, to_hex(crypto:md5_final(Md5Context)) };
        { error, _ } = Error -> Error
    end.

%%% --------------------------------------------------------------------------
%%% Format output to hexa.
to_hex(<<N:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [ N ])).

