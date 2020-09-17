%% A basic reverse polish notation calculator

-module(calc).

-export([rpn/1, rpn_test/0]).

rpn(List) when is_list(List) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(List, " ")),
    Res.

rpn(Symbol, S) ->
    case Symbol of
      "+" ->
          [A, B | Stack] = S,
          [B + A | Stack];
      "-" ->
          [A, B | Stack] = S,
          [B - A | Stack];
      "*" ->
          [A, B | Stack] = S,
          [B * A | Stack];
      "/" ->
          [A, B | Stack] = S,
          [B / A | Stack];
      "^" ->
          [A, B | Stack] = S,
          [math:pow(B, A) | Stack];
      _ ->
          [read(Symbol) | S]
    end.

read(X) ->
    case string:to_float(X) of
      {error, no_float} ->
          list_to_integer(X);
      {Float, _} ->
          Float
    end.

rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try
           rpn("90 34 12 33 55 66 + * - +")
         catch
           error:{badmatch, [_ | _]} ->
               ok
         end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    ok.

