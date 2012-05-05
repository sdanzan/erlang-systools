%%% --------------------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @doc An erlang implementation of the `md5sum` utility.
%%% --------------------------------------------------------------------------

-module(md5sum).
-export([md5sum/1, md5sum/2]).

-define(DEFAULT_BUFFER_LENGTH, 65536).

%%% --------------------------------------------------------------------------
%%% @doc Compute a MD5 checksum of the given file.
md5sum(FileName) -> md5sum(FileName, ?DEFAULT_BUFFER_LENGTH).

%%% --------------------------------------------------------------------------
%%% @doc Compute a MD5 checksum of the given file.
%%%      Use a given length for the read buffer.
%%% @end
md5sum(FileName, BufferLength) ->
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
