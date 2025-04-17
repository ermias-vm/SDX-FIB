# 🚀 Ejecución de Groupy en Erlang

Sigue estos pasos para iniciar, probar y detener el sistema.

## 1. Iniciar los Nodos de Erlang

Para ejecutar `Groupy` de forma distribuida, inicia **cinco nodos** de Erlang, cada uno en una terminal separada. 

Cada nodo representa un proceso en una máquina virtual de Erlang con un nombre único. 

Usa los siguientes comandos en la misma máquina (con `127.0.0.1`):

```sh
erl -name P1@127.0.0.1
erl -name P2@127.0.0.1
erl -name P3@127.0.0.1
erl -name P4@127.0.0.1
erl -name P5@127.0.0.1
```

## 2. Compilar los Módulos 

Desde cualquier terminal anterior compila con estos comandos:

```erlang
c(gms1).
c(gms2).
c(worker).
c(groupy).
c(gui).
```

## 3. Ejecutar y Detener Groupy

### Iniciar Groupy

Desde cualquier terminal uno de los siguientes comandos:

```erlang
groupy:start(gms1, 2000).
groupy:start(gms2, 2000).
```

Parámetros:

- `gms1` Módulo de membresía de grupo usado.
- `2000` Tiempo máximo de espera (en milisegundos) entre mensajes enviados por los workers.

### Detener Groupy

Envía un mensaje stop a los procesos a, b, c, d y e, deteniendo los workers y sus procesos de grupo.

```erlang
groupy:stop().
```

### Detener un Proceso Específico

Para detener un worker individual, por ejemplo, el de P1:

```erlang
groupy:stop({a, 'P1@127.0.0.1'}).
```
