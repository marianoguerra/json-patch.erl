-module(jsonpatch_SUITE).
-compile(export_all).


all() -> [parse_empty_path, parse_slash, parse_one_item, parse_two_items,
          parse_with_number,

          test_json_file].

parse_empty_path(_) ->
    [] = jsonpatch:parse_path(<<"">>).

parse_slash(_) ->
    [<<>>] = jsonpatch:parse_path(<<"/">>).

parse_one_item(_) ->
    [<<"a">>] = jsonpatch:parse_path(<<"/a">>).

parse_two_items(_) ->
    [<<"a">>, <<"b">>] = jsonpatch:parse_path(<<"/a/b">>).

parse_with_number(_) ->
    [<<"a">>, 42, <<"b">>] = jsonpatch:parse_path(<<"/a/42/b">>).

test_json_file(_) ->
    {ok, Data} = file:read_file("../../test/tests.json"),
    JsonTests = jsxn:decode(Data),
    Results = lists:map(fun (#{<<"title">> := Title, <<"input">> := Input,
                               <<"patch">> := Patch}=Test) ->
                ErrorExpected = maps:get(<<"error">>, Test, false),
                Output = maps:get(<<"output">>, Test, nil),
                ParsedPatch = jsonpatch:parse(Patch),
                PatchResult = jsonpatch:patch(ParsedPatch, Input),
                case {ErrorExpected, PatchResult} of
                    {false, {ok, Result}} ->
                        if Result =:= Output ->
                            ct:print("~s: ok", [Title]),
                            true;
                           true ->
                            ct:print("~s result ~p != ~p", [Title, Result, Output]),
                            false
                        end;
                    {true, {ok, Result}} ->
                        ct:print("OK: expected '~s' to fail, got ~p", [Title, Result]),
                        true;
                    {false, Error} ->
                        ct:print("ERROR: unexpected error in ~s, ~p", [Title, Error]),
                        false;
                    {true, Error} ->
                        ct:print("~s failed as expected, ~p", [Title, Error]),
                        true;
                     Other ->
                        ct:print("WAT ~p", [Other]),
                        false
                end
              end,  JsonTests),
    true = lists:all(fun (B) -> B end, Results).
