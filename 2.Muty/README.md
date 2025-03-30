# 🚀 Ejecución de Muty en Erlang

## 1. Iniciar los Workers

Antes de ejecutar Muty, es necesario iniciar cuatro terminales con los siguientes nombres:

```sh
erl -name John@127.0.0.1
erl -name Ringo@127.0.0.1
erl -name Paul@127.0.0.1
erl -name George@127.0.0.1
```
## 2. Iniciar la Terminal Principal

En una nueva terminal, iniciar Muty con el siguiente comando:

```sh
erl -name muty@127.0.0.1
```


## 3. Compilar los Módulos

Dentro de la terminal principal de Muty, compilar los módulos necesarios:

```erlang
c(gui).
c(muty).
c(worker).
c(lock1).
c(lock2).
c(lock3).
```

## 4. Ejecutar Muty

Para iniciar Muty, usar el siguiente comando dentro de la terminal principal:

```erlang
muty:start(Lock, Sleep, Work).
```

Donde:

- `Lock`  es la versión del lock utilizada, puede ser **lock1, lock2 o lock3**.
- `Sleep` es el tiempo de espera en milisegundos.
- `Work`  es el tiempo de trabajo en milisegundos.

Ejemplo:

```erlang
muty:start(lock1, 1000, 2000).
muty:start(lock2, 1000, 2000).
muty:start(lock3, 1000, 2000).
```

## 5. Detener Muty

Para detener Muty, ejecutar:

```erlang
muty:stop().
```
