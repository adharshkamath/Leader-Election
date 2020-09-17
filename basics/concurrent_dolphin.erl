-module(concurrent_dolphin).
-export([dolphin/0]).

dolphin() ->
        receive
            {From, hi_there } ->
                From ! "Hello there!",
                dolphin();
            {From, how_you } ->
                From ! "I'm good. You?",
                dolphin();
            _ -> io:format("New message, eh?")
        end.