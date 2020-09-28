-module(leader_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("../include/state.hrl").

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Timeout = 500,
    Leader = '1@127.0.0.1',
    Nodes0 = ['1@127.0.0.1',
                '2@127.0.0.1',
                '3@127.0.0.1',
                '4@127.0.0.1'],
    Nodes = lists:delete(node(), Nodes0),
    State = #state{leader = Leader, node = node(), nodes = Nodes, timeout = Timeout},
    {ok, { {one_for_one, 5, 10}, [?CHILD(leader, worker, [State])]} }.


