-module(paths).
-compile([debug_info, export_all]).

main([Filename]) ->
    {ok, Data} = file:read_file(Filename),
    Map = parse_data(Data),
    io:format("~p~n", [best_path(Map)]),
    erlang:halt().

parse_data(Data) when is_binary(Data) -> 
    parse_data(binary_to_list(Data));
parse_data(Data) when is_list(Data) ->
    Values = [list_to_integer(Element) || Element <- string:tokens(Data, "\n\t\r ")],
    group_data(Values, []).

group_data([], Ans) ->
    lists:reverse(Ans);
group_data([A, B, C| Rest], Ans) ->
    group_data(Rest, [{A,B,C} | Ans]).

get_choice({A, B, C}, {{DistA, PathA}, {DistB, PathB}}) ->
    Ch1A = {DistA + A, [{a, A} | PathA]},
    Ch2A = {DistB + B + C, [{c, C}, {b, B} | PathA]},
    Ch1B = {DistB + B, [{b, B} | PathB]},
    Ch2B = {DistB + A + C, [{c, C}, {a, A} | PathB]},
    {min(Ch1A, Ch2A), min(Ch1B, Ch2B)}.    

best_path(Map) ->
    {PathA, PathB} = lists:foldl(fun get_choice/2, {{0, []}, {0, []}}, Map),
    {_Distance, Path} = if hd(element(2, PathA)) =/= {x, 0} -> PathA;
                            hd(element(2, PathB)) =/= {x, 0} -> PathB
                        end,
        lists:reverse(Path).