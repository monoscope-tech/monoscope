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

## Useful reading
- Build reload feedback cycle in haskell: https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev 

