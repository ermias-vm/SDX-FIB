-module(lock2).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId);
        stop ->
            ok
    end.

open(Nodes, MyId) ->
    receive
        {take, Master, Ref} ->
            Refs = requests(Nodes, MyId),
            wait(Nodes, Master, Refs, [], Ref, MyId);
        {request, From, Ref, _} ->
            From ! {ok, Ref},
            open(Nodes, MyId);
        stop ->
            ok;
        Error ->
            io:format("open: unsupported message: ~w~n", [Error])
    end.

requests(Nodes, MyId) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId},
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId) ->
    receive
        {request, From, Ref, ReqId} ->
            if 
                ReqId < MyId ->  
                    From ! {ok, Ref},
                    R = make_ref(),
                    From ! {request, self(), R, MyId}, 
                    wait(Nodes, Master, [R|Refs], Waiting, TakeRef, MyId); 
                true ->
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId) 
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId); 
        release ->
            ok(Waiting),            
            open(Nodes, MyId);
        Error ->
            io:format("wait: unsupported message: ~w~n", [Error])
    end.

ok(Waiting) ->
    lists:foreach(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId) ->
    receive
        {request, From, Ref, _} -> 
            held(Nodes, [{From, Ref}|Waiting], MyId);
        release ->
            ok(Waiting),
            open(Nodes, MyId);
        Error ->
            io:format("held: unsupported message: ~w~n", [Error])
    end.