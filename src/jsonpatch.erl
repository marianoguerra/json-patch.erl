-module(jsonpatch).
-export([parse/1, patch/2, parse_path/1]).

parse(RawPatch) when is_binary(RawPatch) ->
    Data = jsxn:decode(RawPatch),
    parse(Data);

parse(Data) -> parse(Data, []).

parse([], Accum) -> {ok, lists:reverse(Accum)};

parse([#{<<"op">> := <<"add">>, <<"path">> := Path, <<"value">> := Val}|T], Accum) ->
    parse(T, [{add, parse_path(Path), Val}|Accum]);
parse([#{<<"op">> := <<"remove">>, <<"path">> := Path}|T], Accum) ->
    parse(T, [{remove, parse_path(Path)}|Accum]);
parse([#{<<"op">> := <<"replace">>, <<"path">> := Path, <<"value">> := Val}|T], Accum) ->
    parse(T, [{replace, parse_path(Path), Val}|Accum]);
parse([#{<<"op">> := <<"move">>, <<"path">> := To, <<"from">> := From}|T], Accum) ->
    parse(T, [{move, parse_path(From), parse_path(To)}|Accum]);
parse([#{<<"op">> := <<"copy">>, <<"path">> := To, <<"from">> := From}|T], Accum) ->
    parse(T, [{copy, parse_path(From), parse_path(To)}|Accum]);
parse([#{<<"op">> := <<"test">>, <<"path">> := Path, <<"value">> := Val}|T], Accum) ->
    parse(T, [{test, parse_path(Path), Val}|Accum]);
parse([Other|_T], _Accum) ->
    {error, {invalidaction, Other}}.

parse_path(PathStr) ->
    lists:map(fun maybe_parse_integer/1, tl(binary:split(PathStr, <<"/">>, [global]))).

% XXX is this slower than catching an exception?
maybe_parse_integer(B) ->
    case re:run(B,"^[0-9]+$") of
        {match, _} -> binary_to_integer(B);
        _ -> B
    end.

patch(Patch, Obj) when is_list(Patch) ->
    dotto:apply(Patch, Obj).
