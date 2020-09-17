-module(abstract).
-compile([debug_info, export_all]).

map(_, []) -> [];
map(Func, [Head|Tail]) -> [Func(Head)|map(Func, Tail)].

filter(Condition, List) -> lists:reverse(filter(Condition, List, [])).

filter(_, [], Ans) -> Ans;
filter(Condition, [Head|Tail], Ans) ->
    case Condition(Head) of
        true -> filter(Condition, Tail, [Head|Ans]);
        false -> filter(Condition, Tail, Ans)
    end.

fold(_, Ans, []) -> Ans;
fold(Func, Ans, [Head|Tail]) -> fold(Func, Func(Head, Start), Tail).        % Book uses Acc which is short for Accumulator, in case you're wondering


% A nice use of folds is to reverse a list
reverse(List) ->
    fold(fun(X, Acc) -> [X|Acc] end, [], List).