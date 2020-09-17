-module(errors).
-compile([debug_info, export_all]).

throws(Func) ->
try Func() of
    _ -> ok
catch 
    Throw -> {throw, not_really_caught, Throw};
    error:Error -> {errs, err_caught, Error};
    exit:Exit -> {exits, exit_seen, Exit}
end.