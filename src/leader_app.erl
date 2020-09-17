-module(leader_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() -> 
    lager:start(),
    ok = application:start(leader),
    ok = connect_nodes(),
    ok.

start(_StartType, _StartArgs) ->
    leader_sup:start_link().

stop(_State) ->
    ok = application:stop(lager),
    ok = application:stop(leader).

connect_nodes() ->
    {ok, AllNodes } = application:get_env(bully, nodes),
    Nodes = lists:delete(node(), AllNodes),
    lager:info("Connecting to nodes ~p~n", [Nodes]),
    ConnResult = lists:map(fun net_kernel:connect_node/1, Nodes),
    case lists:member(true, ConnResult) of
        true ->
            lager:info("Successfully connected to nodes : ~p~n", [nodes()]);
            
        false ->
            lager:info("Failed to connect to any nodes :/~n")
    end,
    ok.