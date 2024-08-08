#!/bin/bash

for protofile in $(find opentelemetry/proto -name "*.proto")
do
    compile-proto-file --out src --includeDir . --proto $protofile
done
