-module(lock3).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId, 0);
        stop ->
            ok
    end.

open(Nodes, MyId, MyClock) ->
    receive
        {take, Master, Ref} ->
            MyNewClock = MyClock + 1,
            Refs = requests(Nodes, MyId, MyNewClock),
            wait(Nodes, Master, Refs, [], Ref, MyId, MyNewClock, MyNewClock);
        {request, From, Ref, _, ReqTime} ->
            MyNewClock = max(MyClock, ReqTime),
            From ! {ok, Ref},
            open(Nodes, MyId, MyNewClock);
        stop ->
            ok;
        {ok, _} ->
            %% delayed ok message
            open(Nodes, MyId, MyClock);
        Error ->
            io:format("open: unsupported message: ~w~n", [Error]),
            open(Nodes, MyId, MyClock)
    end.

requests(Nodes, MyId, MyClock) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId, MyClock},
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId, _, MyClock) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId, MyClock);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyReqTime, MyClock) ->
    receive
        {request, From, Ref, ReqId, ReqTime} ->
            MyNewClock = max(MyClock, ReqTime),
            if (ReqTime < MyReqTime) or ((ReqTime == MyReqTime) and (ReqId < MyId)) ->
                % Si la solicitud entrante tiene prioridad (menor timestamp o menor ID)
                From ! {ok, Ref},
                %From ! {request, self(), make_ref(), MyId, MyNewClock}, % Reenviamos nuestra solicitud
                wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyReqTime, MyNewClock);
            true ->
                % Si nuestra solicitud tiene prioridad, agregamos la otra a la espera
                wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, MyReqTime, MyNewClock)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, MyReqTime, MyClock);
        release ->
            ok(Waiting),            
            open(Nodes, MyId, MyClock);
        Error ->
            io:format("wait: unsupported message: ~w~n", [Error]),
            wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyReqTime, MyClock)
    end.

ok(Waiting) ->
    lists:foreach(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting, MyId, MyClock) ->
    receive
        {request, From, Ref, _, ReqTime} ->
            MyNewClock = max(MyClock, ReqTime),
            held(Nodes, [{From, Ref}|Waiting], MyId, MyNewClock);
        release ->
            ok(Waiting),
            open(Nodes, MyId, MyClock);
        {ok, _} ->
            %% delayed ok message
            held(Nodes, Waiting, MyId, MyClock);
        Error ->
            io:format("held: unsupported message: ~w~n", [Error]),
            held(Nodes, Waiting, MyId, MyClock)
    end.