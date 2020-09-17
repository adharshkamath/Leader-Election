-module(leader).
-behaviour(gen_server).
-export([start_link/1, stop/0, cluster_event/1, get_state/0]).

%% gen_server
-export([init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-include("../include/state.hrl").

start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

stop() ->
    gen_server:cast(?MODULE, stop).

get_state() ->
    {state, State} = gen_server:call(?MODULE, get_state),
    State.

cluster_event({ping, From}) ->
    gen_server:cast(?MODULE, {ping, From}),
    ok;
cluster_event({pong, From}) ->
    gen_server:cast(?MODULE, {pong, From}),
    ok;
cluster_event({announce_election, From}) ->
    gen_server:cast(?MODULE, {announce_election, From}),
    ok;
cluster_event({ok, From}) ->
    gen_server:cast(?MODULE, {ok, From}),
    ok;
cluster_event({new_leader, Leader}) ->
    gen_server:cast(?MODULE, {new_leader, Leader}),
    ok;
cluster_event(Event) ->
    lager:error("Unknown cluster event: ~p", [Event]),
    ok.

init(#state{leader = Leader, node = Node} = State) ->
    Name = case Node of
               Leader ->
                   lager:info("Start leading on ~p", [Node]),
                   multicast(State#state.nodes, {new_leader, Node}),
                   leading;
               _ ->
                   gen_server:cast(?MODULE, ping_leader),
                   pinged
           end,
    {ok, State#state{name = Name}}.

handle_cast(ping_leader, #state{name = Name} = State) when Name =:= pinged ->
    start_pinging(State);

handle_cast(ping_leader, #state{name = Name} = State) when Name =:= pinging ->
    lager:info("Timeout: no pong from leader ~p", [State#state.leader]),
    start_announcing(State);

handle_cast({ping, From}, #state{name = Name} = State) when Name =:= leading ->
    lager:info("Ping from ~p", [From]),
    multicast([From], {pong, State#state.node}),
    {noreply, State};

handle_cast({pong, From}, #state{name = Name} = State) when Name =:= pinging ->
    lager:info("Pong from leader ~p", [From]),
    {noreply, State#state{name = pinged}};

handle_cast({announce_election, From}, #state{name = Name} = State) when Name =:= announcing; Name =:= electing ->
    multicast([From], {ok, State#state.node}),
    {noreply, State};

handle_cast({announce_election, From}, State) ->
    lager:info("Election announce from ~p", [From]),
    multicast([From], {ok, State#state.node}),
    start_announcing(State);

handle_cast(check_announce, #state{name = Name} = State) when Name =:= announcing ->
    lager:info("Announce timeout: no one OK received"),
    start_leading(State);

handle_cast({ok, From}, #state{name = Name} = State) when Name =:= announcing ->
    lager:info("Announce approved from ~p, start electing", [From]),
    send_delayed(State#state.timeout, check_electing),
    {noreply, State#state{name = electing}};

handle_cast(check_electing, #state{name = Name} = State) when Name =:= electing ->
    lager:info("Electing timeout, start announcing again"),
    start_announcing(State);

handle_cast({new_leader, Leader}, State) ->
    lager:info("Set new leader ~p", [Leader]),
    start_pinging(State#state{leader = Leader});

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({Event, From}, #state{name = Name} = State) ->
    lager:warning("Skip event ~p from ~p in state ~p", [Event, From, Name]),
    {noreply, State};

handle_cast(Event, #state{name = Name} = State) ->
    lager:warning("Skip event ~p in state ~p", [Event, Name]),
    {noreply, State}.

handle_info({timer, Event}, State) ->
    gen_server:cast(?MODULE, Event),
    {noreply, State};

handle_info(Request, State) ->
    lager:error("Unexpected info ~p", [Request]),
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, {state, State}, State};

handle_call(Request, From, State) ->
    lager:error("Unexpected call ~p from ~p", [Request, From]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal stuff

multicast(Nodes, Event) ->
    [rpc:cast(Node, leader, cluster_event, [Event]) || Node <- Nodes],
    ok.

node_identity(Node) ->
    {ID, _Host} = string:to_integer(atom_to_list(Node)),
    ID.

send_delayed(Event, Delay) ->
    timer:send_after(Delay, ?MODULE, {timer, Event}),
    ok.

start_leading(#state{node = Node} = State) ->
    lager:info("~p starting to lead~n", [Node]),
    multicast(State#state.nodes, {new_leader, Node}),
    {noreply, State#state{name = pinging}}.

start_pinging(State) ->
    lager:info("Ping leader ~p", [State#state.leader]),
    multicast([State#state.leader], {ping, State#state.node}),
    send_delayed(State#state.timeout * 4, ping_leader),
    {noreply, State#state{name = pinging}}.

start_announcing(#state{node = Node, nodes = Nodes} = State) ->
    Identity = node_identity(Node),
    HigherIdentityNodes = lists:filter(fun(N) -> node_identity(N) > Identity end, Nodes),
    case HigherIdentityNodes of
        [] ->
            start_leading(State);
        HigherIdentityNodes ->
            lager:info("Announce election to ~p", [HigherIdentityNodes]),
            multicast(HigherIdentityNodes, {announce_election, State#state.node}),
            send_delayed(State#state.timeout, check_announce),
            {noreply, State#state{name = announcing}}
    end.        