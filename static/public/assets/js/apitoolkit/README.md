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
    
