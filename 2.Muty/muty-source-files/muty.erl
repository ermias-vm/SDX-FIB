-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter to the start procedure. We also provide the average time (in milliseconds) the worker is going to sleep before trying to get the lock (Sleep) and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    Main = self(),
    spawn('John@127.0.0.1',fun()->register(w1, worker:start("John", Main, Lock, 1, Sleep, Work)) end),
    spawn('Ringo@127.0.0.1',fun()->register(w2, worker:start("Ringo", Main, Lock, 2, Sleep, Work)) end),
    spawn('Paul@127.0.0.1',fun()->register(w3, worker:start("Paul", Main, Lock, 3, Sleep, Work)) end),
    spawn('George@127.0.0.1',fun()->register(w4, worker:start("George", Main, Lock, 4, Sleep, Work)) end),
    collect(4, []).

collect(N, Locks) ->
    if
        N == 0 ->
            lists:foreach(fun(L) -> 
                L ! {peers, lists:delete(L, Locks)} 
            end, Locks);
        true ->
            receive
                {ready, L} ->
                    collect(N-1, [L|Locks])
            end
    end.

stop() ->
     {w1, 'John@127.0.0.1'}  ! stop,
     {w2, 'Ringo@127.0.0.1'}  ! stop,
     {w3, 'Paul@127.0.0.1'}  ! stop,
     {w4, 'George@127.0.0.1'}  ! stop.
