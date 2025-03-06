-module(wait).
-export([hello/0]).

hello() ->
    receive % estat d'espera, es bloquejant
        X  -> io:format("aaa! surprise, a message: ~s~n", [X])
        % X fa match amb qualsevol missatge que arribi, es remplaça ~s per X
        % ~n indica salt de linia
    end.