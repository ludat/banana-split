# 🍌 banana-split

## Para levantar todo ya setupeado

```
docker-compose up -d
ghcid -c='stack repl' --run=':main' --warning
cd ui
pnpm start
```

## Instalación

### Instalar ghcup 

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Con las preguntas que te haga aceptá todos los defaults (instalar stack,
dejar que stack use ghcup para instalar ghc)

Una vez instalado ejecutamos ghcup con

```
ghcup tui
```

Ahí instalar la última version de HLS (actualmente 2.9.0.1)

### Instalar librerías nativas

```
sudo apt install libglpk-dev libpq-dev

```

### Buildear el Backend

```
stack build
```

### Instalar pgroll (para las migraciones)

Vamos a [la página de releases de pgroll](https://github.com/xataio/pgroll/releases)
y nos descargamos el binario correspondiente. 
Una vez descargado lo movemos a un lugar del PATH, por ejemplo en ubuntu:

```
sudo mv pgroll-linux_amd64 /usr/local/bin/pgroll
```

Verifiquemos que esté bien instalado con

```
pgroll --version
```

### Correr las migraciones

Para correr las migraciones corremos el comando:

```
cabal run banana-split -- migrations init
cabal run banana-split -- migrations migrate ./migrations
```

### Correr la base de datos

```
docker-compose up
```

### Correr el Backend

Primero vamos a necesitar instalar ghcid

```
stack install ghcid
```

Luego podremos correr el siguiente comando para tener hotreload

```
ghcid -c='stack repl' --run=':main' --warning
```

En caso de no querer instalar ghcid (y perder el hotreload) podemos levantar el backend utilizando 

```
stack run
```

### Correr el Frontend

Entramos a la carpeta de ui y corremos 

 ```
pnpm install
pnpm start
 ```

