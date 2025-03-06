-module(pong).
-export([start/0]).

start() ->
    register(pong, self()), % Registra el proceso bajo el nombre "pong"
    io:format("Pong process started with PID ~p~n", [self()]),
    loop().

loop() ->
    receive
        {ping, From} ->
            io:format("Received ping from ~p~n", [From]),
            From ! pong, % Responde con "pong"
            loop();
        stop ->
            io:format("Stopping pong process~n"),
            ok;
        _ ->
            io:format("Unknown message received~n"),
            loop()
    end.