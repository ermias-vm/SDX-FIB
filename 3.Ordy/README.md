# 🚀 Ejecución de Ordy en Erlang

## 1. Iniciar los Workers

Antes de ejecutar Ordy, es necesario iniciar cuatro terminales con los siguientes nombres:

```sh
erl -name p1@127.0.0.1
erl -name p2@127.0.0.1
erl -name p3@127.0.0.1
erl -name p4@127.0.0.1
```

## 2. Compilar los Módulos (por ejemplo, desde p1)

En la terminal principal (p1), compila los módulos necesarios:

```erlang
c(ordy).
c(worker).
c(seq).
c(basic).
c(total).
c(causal).
```

## 3. Ejecutar Ordy

Para iniciar Ordy, usa el siguiente comando desde cualquiera de las 4 terminales:

```erlang
ordy:start(type, sleep, jitter, duration).
```

Donde:

- `type` es la variante de ejecución, que puede ser **basic, causal o total**.
- `sleep` es el tiempo de espera entre ejecuciones, en milisegundos.
- `jitter` es el retraso aleatorio, en milisegundos.
- `duration` es la duración total de la ejecución, en milisegundos.

Ejemplo:

```erlang
ordy:start(basic,1000,1000,3000).
ordy:start(causal,1000,1000,3000).
ordy:start(total,1000,1000,3000).
```
