-module(lock1).
-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(_) ->
    receive
        {peers, Nodes} ->
            open(Nodes);
        stop ->
            ok
    end.

open(Nodes) ->
    receive
        {take, Master, Ref} ->  % Un Worker solicita el lock
            Refs = requests(Nodes), % Se pide permiso a todos los nodos
            wait(Nodes, Master, Refs, [], Ref);
        {request, From,  Ref} ->
            From ! {ok, Ref},
            open(Nodes);
        stop ->
            ok;
	{ok, _} ->
            %% delayed ok message
            open(Nodes);
        Error ->
            io:format("open: unsupported message: ~w~n", [Error]),
            open(Nodes)
    end.

requests(Nodes) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R}, 
        R 
      end, 
      Nodes).

% Caso base: lista de referencias vacia (todos los nodos han respondido con {ok, Ref}).
wait(Nodes, Master, [], Waiting, TakeRef) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting);

wait(Nodes, Master, Refs, Waiting, TakeRef) ->
    receive
        {request, From, Ref} -> % Si se recibe una solicitud se agrega el worker solicitan a la lista de espera.
            % Si otro candado envía una solicitud {request, From, Ref}, se agrega el solicitante 
            % (From) junto con su referencia (Ref) a la lista Waiting. 
            % Si varios candados envían solicitudes al mismo tiempo (por ejemplo, dos trabajadores 
            % intentan tomar sus candados simultáneamente), se produce un deadlock. Esto ocurre 
            % porque, mientras un candado está en el estado 'wait', no responde inmediatamente 
            % con {ok, Ref} a las solicitudes recibidas, sino que las acumula en Waiting y 
            % continúa esperando las respuestas {ok, Ref} de sus propias solicitudes enviadas 
            % (almacenadas en Refs). Si los candados involucrados están todos en 'wait', ninguno 
            % avanza, ya que cada uno espera una respuesta que el otro no puede enviar, 
            % creando una dependencia circular.
            wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef);
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs), % Eliminamos la referencia de la lista de referencias.
            wait(Nodes, Master, NewRefs, Waiting, TakeRef);
        release ->  % El worker aborta antes de obtener el lock
            ok(Waiting), % Envia ok todo los workers que estaban en waiting           
            open(Nodes);
	Error ->
            io:format("wait: unsupported message: ~w~n", [Error]),
            wait(Nodes, Master, Refs, Waiting, TakeRef)
    end.

ok(Waiting) ->
    lists:foreach(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting) ->
    receive
        {request, From, Ref} ->
            held(Nodes, [{From, Ref}|Waiting]);
        release ->
            ok(Waiting),
            open(Nodes);
	{ok, _} ->
            %% delayed ok message
            held(Nodes, Waiting);
	Error ->
            io:format("held: unsupported message: ~w~n", [Error]),
            held(Nodes, Waiting)
    end.
