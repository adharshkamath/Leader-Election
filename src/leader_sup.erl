-module(leader_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("../include/state.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Timeout} = application:get_env(leader, timeout),
    {ok, Leader} = application:get_env(leader, lead),
    {ok, Nodes0} = application:get_env(leader, nodes),
    Nodes = lists:delete(node(), Nodes0),
    State = #state{leader = Leader, node = node(), nodes = Nodes, timeout = Timeout},
    {ok, { {one_for_one, 5, 10}, [?CHILD(leader, worker, [State])]} }.


