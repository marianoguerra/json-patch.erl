-module(jsonpatch_SUITE).
-compile(export_all).


all() -> [parse_empty_path, parse_slash, parse_one_item, parse_two_items,
          parse_with_number, parse_wrong_patch_fails,
          test_spec_tests, test_json_tests].

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

parse_wrong_patch_fails(_) ->
    BadAction = #{name => "bob"},
    GoodAction = #{<<"op">> => <<"test">>, <<"path">> => <<"/a">>, <<"value">> => 12},
    {error, {invalidformat, 42}} = jsonpatch:parse(<<"42">>),
    {error, {invalidaction, BadAction}} = jsonpatch:parse([BadAction]),
    {error, {invalidaction, BadAction}} = jsonpatch:parse([BadAction, GoodAction]),
    {error, {invalidaction, BadAction}} = jsonpatch:parse([GoodAction, BadAction]),
    {error, {invalidaction, BadAction}} = jsonpatch:parse([GoodAction, BadAction, GoodAction]).

test_json_tests(_) ->
    test_json_file("../../test/tests.json").

test_spec_tests(_) ->
    test_json_file("../../test/spec_tests.json").

% this is not a top level test
test_json_file(Path) ->
    {ok, Data} = file:read_file(Path),
    JsonTests = jsx:decode(Data, [return_maps]),
    Results = lists:map(fun (#{<<"doc">> := Input, <<"patch">> := Patch}=Test) ->
                Title = maps:get(<<"comment">>, Test, <<"test">>),
                ErrorVal = maps:get(<<"error">>, Test, false),
                ErrorExpected = ErrorVal /= false,
                Output = maps:get(<<"expected">>, Test, nooutput),
                Disabled = maps:get(<<"disabled">>, Test, false),

                if Disabled ->
                       ct:print("SKIP: disabled test '~s'", [Title]),
                       true;
                   true ->
                       PatchResult = try
                                         {ok, ParsedPatch} = jsonpatch:parse(Patch),
                                         jsonpatch:patch(ParsedPatch, Input)
                                     catch
                                        Type:ExError -> {error, {Type, ExError}}
                                     end,

                       case {ErrorExpected, PatchResult} of
                           {false, {ok, Result}} ->
                               if Output =:= nooutput orelse Result =:= Output ->
                                      ct:print("OK: ~s", [Title]),
                                      true;
                                  true ->
                                      ct:print("ERROR: ~s result ~p != ~p", [Title, Result, Output]),
                                      false
                               end;
                           {true, {ok, Result}} ->
                               ct:print("OK: expected '~s' to fail, got ~p", [Title, Result]),
                               true;
                           {false, Error} ->
                               ct:print("ERROR: unexpected error in ~s, ~p", [Title, Error]),
                               false;
                           {true, Error} ->
                               ct:print("OK: ~s ~p failed as expected, ~p", [Title, ErrorVal, Error]),
                               true;
                           Other ->
                               ct:print("WAT: ~p", [Other]),
                               false
                       end
                end
              end,  JsonTests),
    true = lists:all(fun (B) -> B end, Results).
