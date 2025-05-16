# 🌐 Ejecución de Namy en Erlang

Sigue estos pasos para iniciar, probar y detener el sistema DNS.

## 1. Iniciar los Servidores DNS

Para implementar la jerarquía DNS, inicia **tres nodos** de Erlang, cada uno en una terminal separada:

```sh
# Servidor root
erl -name root@127.0.0.1 -setcookie dns

# Servidor edu
erl -name edu@127.0.0.1 -setcookie dns

# Servidor upc
erl -name upc@127.0.0.1 -setcookie dns
```

## 2. Compilar los Módulos

Compila los módulos en cualquiera de las terminales. Solo es necesario hacerlo una vez:

```erlang
c(server).
c(resolver).
c(entry).
c(host).
c(namy).
c(cache).
```

## 3. Iniciar los Servidores

### Servidor Root
En la terminal `root@127.0.0.1`:

```erlang
server:start().
```

### Servidor Edu
En la terminal `edu@127.0.0.1`:

```erlang
server:start(edu, {server, 'root@127.0.0.1'}).
```

### Servidor UPC
En la terminal `upc@127.0.0.1`:

```erlang
server:start(upc, {server, 'edu@127.0.0.1'}).
```

## 4. Iniciar los Hosts

Abre una nueva terminal para los hosts:

```sh
erl -name hosts@127.0.0.1 -setcookie dns
```

Registra algunos hosts en el servidor UPC:

```erlang
host:start(www, www, {server, 'upc@127.0.0.1'}).
host:start(ftp, ftp, {server, 'upc@127.0.0.1'}).
```

## 5. Iniciar el Cliente con Resolver

Abre una terminal para el cliente:

```sh
erl -name client@127.0.0.1 -setcookie dns
```

Inicia el resolver:

```erlang
resolver:start({server, 'root@127.0.0.1'}).
```

## 6. Realizar Consultas DNS

Haz consultas a nombres de host:

```erlang
namy:ping([www,upc,edu], resolver).
namy:ping([ftp,upc,edu], resolver).
```

## 7. Configurar TTL (Time-To-Live)

Configura el tiempo de caducidad para las entradas:

```erlang
{server, 'root@127.0.0.1'} ! {ttl, 60}.
{server, 'edu@127.0.0.1'} ! {ttl, 60}.
{server, 'upc@127.0.0.1'} ! {ttl, 60}.
```

## 8. Operaciones de Mantenimiento

### Purgar la Caché
Para limpiar la caché del resolver:

```erlang
resolver ! purge.
```

### Detener Hosts
Para detener los hosts:

```erlang
host:stop(www).
host:stop(ftp).
```

### Verificar Estado del Servidor
Para ver las entradas registradas en un servidor:

```erlang
{server, 'root@127.0.0.1'} ! status.
```

### Detener un Servidor
Para detener un servidor:

```erlang
server:stop().
```

