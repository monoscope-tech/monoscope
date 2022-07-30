# TODO

### In progress
- [] Documentation UI on the landing page
- [] Ability to configure fields to be skipped
- [] Document the log query language
- [] List apitoolkit on g2.com, producthunt.com, etc
- [] Integrate paddle for payment with volume based pricing [track total requests per month]
  - [] Track requests per month over time 
  - [] Track reqs per second and reqs per minute per month over time
  - [] Merge field format anomalies into shapes
    - [] improve anomalies UX as much as possible
- [] improve querying.
  - [] click a field in log explorer. 
- [] When you log in with auth0, override the existing name and photos with the latest from auth0
- [] Add concept of super admins who can see all projects on their platforms and who can log into any project.

#### 26/7/2022
- [] Hash each shape, and only insert new shapes if there has been a lot of changes. Else, only the request dump is good enough. 
- [] Introduce caching for holding the projects in memory and the list of their shapes.
  - [] Using the cache and the hashes, prevent unnecessary db inserts, and only insert for requests that don't match existing shapes, or when we don't yet have enough examples.
- [] map through fields and generate their hashes. Also set the endpoint hash in there
  
### Completed Column ✓

### Unspecified
