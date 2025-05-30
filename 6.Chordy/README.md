# Chordy - Implementación de Tabla Hash Distribuida

Este directorio contiene la implementación de una tabla hash distribuida basada en el protocolo Chord con tolerancia a fallos y capacidades de replicación.

## Descripción General

Chordy implementa una tabla hash distribuida usando el protocolo Chord. La implementación progresa a través de varias versiones:
- `node1.erl`: Anillo Chord básico con estabilización
- `node2.erl`: Añade capacidades de almacenamiento distribuido
- `node3.erl`: Incluye detección y recuperación de fallos
- `node4.erl`: Implementa replicación para tolerancia a fallos



## Instrucciones de Configuración

### 1. Configuración Inicial

Navega al directorio de archivos fuente de chordy:

Inicia una shell de Erlang y compila todos los módulos:
```erlang
erl
c(key).
c(storage).
c(chordy).
c(node1).
c(node2).
c(node3).
c(node4).
```


## Pruebas de Cada Implementación

### Node1 - Anillo Chord Básico

Prueba formación básica del anillo y estabilización:

```erlang
% Crear primer nodo
N2 = node1:start(2).

% Añadir más nodos para formar un anillo
N4 = node1:start(4, N2).
N0 = node1:start(0, N2).
N3 = node1:start(3, N2).
N1 = node1:start(1, N2).

% Enviar sonda para ver estructura del anillo
N2 ! probe.

% Detener nodos
N1 ! stop.
N3 ! stop.
```

### Node2 - Almacenamiento Distribuido

Prueba operaciones de almacenamiento:

```erlang
% Crear anillo con almacenamiento
N2 = node2:start(2).

% Conectar cliente para añadir/buscar datos
P = chordy:connect(N2).

% Añadir pares clave-valor
P ! {add, 0, 0}.
P ! {add, 1, 1}.
P ! {add, 2, 2}.

% Comprobar estado del anillo y almacenamiento
N2 ! probe.

% Añadir más nodos
N4 = node2:start(4, N2).
N2 ! probe.

N0 = node2:start(0, N2).
N2 ! probe.

N3 = node2:start(3, N2).
N2 ! probe.

N1 = node2:start(1, N2).
N2 ! probe.

% Probar búsquedas
P ! {add, 3, 3}.
P ! {lookup, 3}.

% Sonda final para ver distribución
N2 ! probe.
```

### Node3 - Detección de Fallos

Prueba tolerancia a fallos:

```erlang
% Crear anillo
N2 = node3:start(2).
N4 = node3:start(4, N2).
N0 = node3:start(0, N2).
N3 = node3:start(3, N2).
N1 = node3:start(1, N2).

% Comprobar anillo inicial
N2 ! probe.

% Simular fallos y observar recuperación
N1 ! stop.
N2 ! probe.

N3 ! stop.
N2 ! probe.

N0 ! stop.
N2 ! probe.

N4 ! stop.
N2 ! probe.
```

### Node4 - Replicación y Tolerancia a Fallos Avanzada

Prueba replicación automática y recuperación robusta:

```erlang
% Crear anillo con replicación
N2 = node4:start(2).
N4 = node4:start(4, N2).
N0 = node4:start(0, N2).
N3 = node4:start(3, N2).
N1 = node4:start(1, N2).

% Comprobar anillo inicial y réplicas
N2 ! probe.

% Conectar cliente para operaciones de almacenamiento
P = chordy:connect(N2).

% Añadir datos que se replicarán automáticamente
P ! {add, 10, "dato_10"}.
P ! {add, 20, "dato_20"}.
P ! {add, 30, "dato_30"}.

% Verificar distribución y replicación
N2 ! probe.

% Simular fallo de nodo que almacena datos
N1 ! stop.

% Verificar que los datos persisten por replicación
P ! {lookup, 10}.
N2 ! probe.

% Simular múltiples fallos para probar robustez
N3 ! stop.
N2 ! probe.

% Los datos deben seguir disponibles
P ! {lookup, 20}.
P ! {lookup, 30}.

% Añadir nuevo nodo para ver redistribución automática
N5 = node4:start(5, N2).
N2 ! probe.

% Verificar que las réplicas se redistribuyen
P ! {add, 40, "dato_40"}.
P ! {lookup, 40}.
N2 ! probe.
```


## Funciones y Mensajes Clave

### Interfaz Cliente
- `{add, Key, Value}` - Añadir par clave-valor vía cliente chordy
- `{lookup, Key}` - Buscar valor por clave vía cliente chordy

### Mensajes de Nodo
- `{add, Key, Value, Qref, Client}` - Añadir directamente al nodo
- `{lookup, Key, Qref, Client}` - Buscar directamente desde nodo
- `probe` - Mostrar estructura del anillo y estado del almacenamiento
- `stop` - Detener un nodo de forma elegante

### Mensajes de Replicación (Node4)
- `{replicate, Key, Value, Qref, Client}` - Replicar datos con confirmación al cliente
- `{pushreplica, NewReplica}` - Enviar réplica completa al sucesor
- `{handover, Elements}` - Transferir responsabilidad por rango de claves

## Conceptos Clave

### Distribución de Almacenamiento
- Cada nodo es responsable de las claves en el rango (Predecesor, MiClave]
- Las claves se distribuyen usando hash consistente
- Los nuevos nodos automáticamente se hacen cargo de parte del espacio de claves

### Tolerancia a Fallos
- Los nodos monitorean su predecesor y sucesor usando referencias de monitor
- Los nodos fallidos se detectan automáticamente vía mensajes `{'DOWN', Ref, process, _, _}`
- El anillo se repara automáticamente redirigiendo las conexiones
- **Node4**: La replicación asegura persistencia de datos durante fallos múltiples
- **Node4**: Cada nodo mantiene una réplica de los datos de su predecesor
- **Node4**: Al detectar un fallo, el nodo fusiona su almacén principal con la réplica

### Estabilización
- La estabilización periódica mantiene la integridad del anillo
- Los nuevos nodos se integran a través del protocolo notify/stabilize
- Maneja uniones concurrentes y particiones de red
