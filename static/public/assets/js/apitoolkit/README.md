# Query editor implementation

To have the most succinct and extendable editor/builder. 

The page will be loaded with only the full apitoolkit query. And that info should be enough to render the query editor. 
Example queries:
    request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
    method==\"GET\" | traces  // Source is set to traces. Can be requests, logs, traces, spans, metrics 
    method == "GET" | stats count(*) 
    method == "GET" | stats count(*) as total by field1
    method == "GET" | stats count(*) as total by field1, field2
    method == "GET" | timechart 
    method == "GET" | timechart [1d]
    method == "GET" | timechart count(*) [1d]
    method == "GET" | timechart count(*) by field1, field2 [1d]
    

We have 2 implementation options:
1. We could parse the query client time into an object. Allquery builder actions can mutate the object. 
    Then we ccan write code to convert that object into the actual query. 
    Challenge is reimplementing all that parsing logic in js, when its sort of in the haskell already. 

2. We can JSON encode the AST, and encode it into the html, then use it as before. But when the browser needs the text query, eg after a query builder update
    we're forced to make a call to the backend to exchange the ast for actual query. 
    2.a. We can make it such that the ast is also submitted when requesting explorer page. but this makes the log list endpoint less flexible. since its used around the app to show logs and the likes. sticking to just a query would be nice. 

Solution: An endpoint that swaps AST to query and query to AST. This feels wrong, since if a user does sth in the page, 
    we would be making 2 http reqs. 

We can switch the explorer page, such that the editor state is stored in a json which is in the query params
