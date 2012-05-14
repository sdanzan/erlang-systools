%%% --------------------------------------------------------------------------
%%% @author Serge Danzanvilliers <serge.danzanvilliers@gmail.com>
%%% @doc Compute file checksums.
%%%      Wrappers functions over the md5sum, sha1sum, sha256, and sha512sum 
%%%      utilities are available and may be a bit faster.
%%% @end
%%% --------------------------------------------------------------------------

-module(checksums).
-export([md5sum/1, sha1sum/1, sha256sum/1, sha512sum/1]).
-export([md5sum/2, sha1sum/2, sha256sum/2, sha512sum/2]).

%% Buffer length when reading files
-define(DEFAULT_BUFFER_LENGTH, 65536).

%% --------------------------------------------------------------------------
-spec md5sum(string()) -> string() | { error, string() }.

%% @doc MD5 digest on file.
md5sum(FileName) -> md5sum(FileName, string).

-spec md5sum(FileName :: string(),
             string | binary | fast) -> string() | { error, string() }.
%% @doc MD5 digest on file.
%%      If second parameter is 'fast', will use external 'md5sum' utility.
%% @end
md5sum(FileName, fast) -> fastsum(FileName, "md5sum");
md5sum(FileName, Type) -> 
    gensum(FileName, Type,
           { fun crypto:md5_init/0,
             fun crypto:md5_update/2, 
             fun crypto:md5_final/1 }).

%% --------------------------------------------------------------------------
-spec sha1sum(string()) -> string() | { error, string() }.

%% @doc SHA1 digest on file.
sha1sum(FileName) -> sha1sum(FileName, string).

-spec sha1sum(FileName :: string(),
              string | binary | fast) -> string() | { error, string() }.
%% @doc SHA1 digest on file.
%%      If second parameter is 'fast', will use external 'sha1sum' utility.
%% @end
sha1sum(FileName, fast) -> fastsum(FileName, "sha1sum");
sha1sum(FileName, Type) -> 
    gensum(FileName, Type,
           { fun crypto:sha_init/0,
             fun crypto:sha_update/2, 
             fun crypto:sha_final/1 }).

%% --------------------------------------------------------------------------
-spec sha256sum(string()) -> string() | { error, string() }.

%% @doc SHA256 digest on file.
sha256sum(FileName) -> sha256sum(FileName, string).

-spec sha256sum(FileName :: string(),
                string | binary | fast) -> string() | { error, string() }.
%% @doc SHA256 digest on file.
%%      If second parameter is 'fast', will use external 'sha256sum' utility.
%% @end
sha256sum(FileName, fast) -> fastsum(FileName, "sha256sum");
sha256sum(FileName, Type) -> 
    gensum(FileName, Type,
           { fun crypto:sha256_init/0,
             fun crypto:sha256_update/2, 
             fun crypto:sha256_final/1 }).

%% --------------------------------------------------------------------------
-spec sha512sum(string()) -> string() | { error, string() }.

%% @doc SHA512 digest on file.
sha512sum(FileName) -> sha512sum(FileName, string).

-spec sha512sum(FileName :: string(),
                string | binary | fast) -> string() | { error, string() }.
%% @doc SHA512 digest on file.
%%      If second parameter is 'fast', will use external 'sha512sum' utility.
%% @end
sha512sum(FileName, fast) -> fastsum(FileName, "sha512sum");
sha512sum(FileName, Type) -> 
    gensum(FileName, Type,
           { fun crypto:sha512_init/0,
             fun crypto:sha512_update/2, 
             fun crypto:sha512_final/1 }).

%% --------------------------------------------------------------------------

%% --------------------------------------------------------------------------
%% Launch a xxxsum utility and get back the result.
fastsum(FileName, Cmd) ->
    FullCmd = Cmd ++ " " ++ shell_utils:quote(FileName) ++ "; echo $?",
    case string:tokens(os:cmd(FullCmd), "\n") of
        [ Output, "0" ] -> hd(string:tokens(Output, " "));
        [ Error | _ ]   -> { error, Error }
    end.

%% --------------------------------------------------------------------------
%% Compute a checksum of the given file using plain erlang.
%% Use a given length for the read buffer.
gensum(FileName, Type, { FInit, FUpdate, FFinal }) ->
    { ok, File } = file:open(FileName, 
                             [ read, binary, raw, 
                               { read_ahead, ?DEFAULT_BUFFER_LENGTH * 4 } ]),
    try gensum_loop(File, Type, { FUpdate, FFinal }, FInit())
    after file:close(File) end.

%% --------------------------------------------------------------------------
%% Checksum computation loop.
gensum_loop(File, Type, { FUpdate, FFinal }, Context) ->
    case file:read(File, ?DEFAULT_BUFFER_LENGTH) of
        { ok, Data } -> gensum_loop(File,
                                    Type,
                                    { FUpdate, FFinal },
                                    FUpdate(Context, Data));
        eof -> 
            Digest = FFinal(Context),
            if Type =:= binary -> Digest; true -> to_hex(Digest) end;
        { error, _ } = Error -> Error
    end.

%% --------------------------------------------------------------------------
%% Format output to hexa.
to_hex(BitsString) ->
    Size = bit_size(BitsString),
    <<N:Size/big-unsigned-integer>> = BitsString,
    Format = "~" ++ integer_to_list(Size div 4) ++ ".16.0b",
    lists:flatten(io_lib:format(Format, [ N ])).

