# apitoolkit-server

- Install and setup haskell via ghcup

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- clone this repository
- install postgres with timescaledb support https://docs.timescale.com/install/latest/
- Install LLVM
- gcloud auth application-default login

Mac

```
brew install llvm
```

Linux

```
sudo apt-get install llvm
```

- Install timescale db and the timescaledb toolkit https://docs.timescale.com/self-hosted/latest/install/

- Install Libpq

Mac

```
brew install libpq
```

Linux

```
sudo apt-get install libpq-dev
```

- Run the apitoolkit project via `stack run`, from the root of the project. Stack should have been installed via the ghcup step.

## Useful links

- There is now a page for manual data ingestion available at `/p/<project-id>/manual_ingest`. This link isn't added to the menu since it's for dev testing only.
- https://lightstep.com/ and datadog and https://www.instana.com/ For their observability. We should learn from them. Lightstep has nice landing page

## Useful tools

- install ormolu (brew install ormolu), for fmt
- install hlint for linting (brew install hlint)
- `make fmt` will use ormolu to format the code
- `make lint` will run the linter and show you items to fix
- it's recommended to compile haskell language server locally,else it crashes, especially on mac
  due to this issue: https://github.com/haskell/haskell-language-server/issues/2391

## Useful reading

- Build reload feedback cycle in haskell: https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev

# To build the service worker, run

workbox generateSW workbox-config.js

## Run the timescale database via docker

- Install docker via the docker website
- setup a docker volume to hold the postgres data: `docker volume create pgdata`
- run the docker image

```
    make timescaledb-docker
```

- apitoolkit depends on pg_cron so you would need to update the postgres config in your pgdata volume to include the following;
```
shared_preload_libraries = 'pg_cron'
cron.database_name = 'apitoolkit'
```
alternatively you can run the following sql commands against the database
```sql
ALTER system SET cron.database_name = 'apitoolkit';
ALTER system SET shared_preload_libraries = 'pg_cron';
```
- restart timescaledb-docker
