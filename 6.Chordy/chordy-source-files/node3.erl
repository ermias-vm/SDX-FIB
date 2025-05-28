-module(node3).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),
    Next = %% ADD
    Store =  %%ADD
    node(MyKey, Predecessor, Successor, Next, Store).

connect(MyKey, nil) ->
    {ok, {%%ADD,%%ADD ,%%ADD}};
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {%%ADD,%%ADD ,%%ADD}};
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Next, Store) ->
    receive 
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Next, Store);
        {notify, NewPeer} ->
            {NewPredecessor, NewStore} = notify(NewPeer, MyKey, Predecessor, Store), %%added
            node(MyKey, NewPredecessor, Successor, Next, NewStore);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Next, Store);
        {status, Pred, Nx} ->
            %%ADD = stabilize(Pred, Nx, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, NewNext, Store);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Next, Store);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Added);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        {handover, Elements} ->
            NewStore = storage:merge(Store, Elements),
            node(MyKey, Predecessor, Successor, Next, NewStore);
        {'DOWN', Ref, process, _, _} ->
            {NewPred, NewSucc, NewNext} = down(Ref, Predecessor, Successor, Next),
            node(MyKey, NewPred, NewSucc, NewNext, Store);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Next, Store);
        {probe, RefKey, Nodes, T} ->
            forward_probe(MyKey, RefKey, [MyKey|Nodes], T, Successor, Store),
            node(MyKey, Predecessor, Successor, Next, Store);
        Error ->
            io:format("Reception of strange message ~w~n", [Error]),
            node(MyKey, Predecessor, Successor, Next, Store)
   end.

stabilize(Pred, Next, MyKey, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
      nil ->
          %% ADD
          {Successor, Next};
      {MyKey, _} ->
          {Successor, Next};
      {Skey, _} ->
          %% ADD
          {Successor, Next};
      {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    %% ADD linea 1
                    %% ADD linea 2
                    %% ADD linea 3
                false ->
                    %% ADD linea 1
                    %% ADD linea 2
            end
    end.

stabilize( %%ADD ) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, %% ADD) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
            %% ADD ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Store, MyKey, Nkey, Npid),
            %% ADD
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    Keep = %% ADD
                    %% ADD
                    %% ADD
                false -> 
                    %% ADD
            end
    end.

add(Key, Value, Qref, Client, _, nil, %% aDD, Store) ->
    %% ADD
    Store;
add(Key, Value, Qref, Client, MyKey, %% ADD, %%ADD , Store) ->
    case key:between(%%add, %%add, %%add) of
        true ->
              Added = %% ADD, 
              Client ! {Qref, ok},
              Added;
        false ->
              %% ADD
              Store
    end.
    
lookup(Key, Qref, Client, _, nil, %% ADD, _) ->
    %% ADD;
lookup(Key, Qref, Client, MyKey, %%ADD, %%ADD , Store) ->
    case key:between(%%add, %%add, %%add) of
        true ->
            Result = %% ADD,
            Client ! {Qref, Result};
        false ->
            %% ADD
    end.
    
handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = %% ADD,
    Npid ! {handover, Leave},
    Keep.

monit(Pid) ->
    erlang:monitor(process, Pid).

demonit(nil) ->
    ok;
demonit(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next}; 
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    %% ADD
    {Predecessor, %%ADD , nil}.
    
create_probe(MyKey, %% ADD , Store) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Node ~w created probe -> Store: ~w~n", [MyKey, Store]).
	
remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2-T, native, microsecond),
    io:format("Node ~w received probe after ~w us -> Ring: ~w~n", [MyKey, Time, Nodes]).
	
forward_probe(MyKey, RefKey, Nodes, T, %% ADD, Store) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Node ~w forwarded probe started by node ~w -> Store: ~w~n", [MyKey, RefKey, Store]).