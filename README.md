# 🍌 banana-split

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

### Instalar Reshape (para las migraciones)

Vamos a [la página de releases de reshape](https://github.com/fabianlindfors/reshape/releases) 
y nos descargamos el binario correspondiente. 
Una vez descargado lo movemos a un lugar del PATH, por ejemplo en ubuntu:

```
sudo mv reshape-linux_amd64 /usr/local/bin/reshape
```

Verifiquemos que esté bien instalado con

```
reshape --version
```

### Correr las migraciones

Para correr las migraciones corremos el comando:

```
reshape migration start --url postgres://postgres:postgres@127.0.0.1/bananasplit_dev
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

### Instalar el Frontend

Entramos a la carpeta de ui y corremos 

 ```
pnpm install 
pnpm start
 ```