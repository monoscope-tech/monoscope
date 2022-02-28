# apitoolkit-server

- Install and setup haskell via ghcup
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
- clone this repository
- install postgres with timescaledb support https://docs.timescale.com/install/latest/
- Install LLVM 
- gcloud auth application-default login
```
brew install llvm
```
- Install timescale db and the timescaledb toolkit https://docs.timescale.com/timescaledb/latest/how-to-guides/hyperfunctions/install-toolkit/#install-toolkit-on-self-hosted-timescaledb

- Install Libpq
```
brew install libpq
```
- Run the apitoolkit project via `stack run`, from the root of the project. Stack should have been installed via the ghcup step.

##
- To generate dummy request menssages into the queue for the haskell apps to consume 
```
go test -run=TestAPIToolkitWorkflow
```

## Useful links
- There is now a page for manual data ingestion available at `/p/<project-id>/manual_ingest`. This link isn't added to the menu since it's for dev testing only.

## Useful tools
- install ormolu (brew install ormolu), for fmt 
- install hlint for linting (brew install hlint)
- `make fmt` will use ormolu to format the code 
- `make lint` will run the linter and show you items to fix
- it's recommended to compile haskell language server locally,else it crashes,  especially on mac
  due to this issue: https://github.com/haskell/haskell-language-server/issues/2391


## Useful reading
- Build reload feedback cycle in haskell: https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev 

