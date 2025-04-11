-module(groupy).
-export([start/2, stop/0, stop/1]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep) ->
    spawn('P1@127.0.0.1', fun() -> register(a, worker:start("P1", Module, Sleep)) end),
    spawn('P2@127.0.0.1', fun() -> register(b, worker:start("P2", Module, {a, 'P1@127.0.0.1'}, Sleep)) end),
    spawn('P3@127.0.0.1', fun() -> register(c, worker:start("P3", Module, {b, 'P2@127.0.0.1'}, Sleep)) end),
    spawn('P4@127.0.0.1', fun() -> register(d, worker:start("P4", Module, {c, 'P3@127.0.0.1'}, Sleep)) end),
    spawn('P5@127.0.0.1', fun() -> register(e, worker:start("P5", Module, {d, 'P4@127.0.0.1'}, Sleep)) end).

stop() ->
    stop({a, 'P1@127.0.0.1'}),
    stop({b, 'P2@127.0.0.1'}),
    stop({c, 'P3@127.0.0.1'}),
    stop({d, 'P4@127.0.0.1'}),
    stop({e, 'P5@127.0.0.1'}).

stop(Name) ->
    if is_tuple(Name) ->
        Name ! stop;
    true ->
        case whereis(Name) of
            undefined ->
                ok;
            Pid ->
                Pid ! stop
        end
    end.