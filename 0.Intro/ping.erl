-module(ping).
-export([start/1]).

start(NodeName) ->
    PongPid = {pong, NodeName}, % Crea un tuple con el nombre del proceso y el nodo
    PongPid ! {ping, self()},   % Envía un mensaje al proceso "pong" en el nodo especificado
    receive
        pong ->
            io:format("Received pong from ~p~n", [NodeName])
    after 5000 ->
        io:format("No response from ~p~n", [NodeName])
    end.