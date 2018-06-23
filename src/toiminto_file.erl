-module(toiminto_file).

-export([try_parse/1]).

% --- API ------------------------------------------------------------------------------------------
try_parse(File) ->
    case files_dir() of
        ok ->
            parse(File);
        Errors ->
            Errors

    end.

% --- Internal -------------------------------------------------------------------------------------

files_dir() ->
    try file:make_dir("files") of
        ok ->
            ok;
        {error, eexist} ->
            ok
    catch
        error:Error ->
            Error;
        exit:Exit ->
            Exit;
        throw:Throw ->
            Throw
    end.

parse(File) ->
    try file:read_file(File) of
        {ok, Binary} ->
            parse(binary, Binary)
    catch
        _:_ ->
            handle_error
    end.

parse(binary, Binary) ->
    L = binary_to_list(Binary),
    {ok, S, _} = toiminto_tokenizer:string(L),
    {ok, P} = toiminto_grammar:parse(S),
    P.
