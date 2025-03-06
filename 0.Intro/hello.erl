-module(hello). % defineix nom del modul
-export([world/0]). % exporta la funcio hello_world amb 0 arguments 
                    % sera visible des de fora del modul

world() ->
    "Hello, World!".