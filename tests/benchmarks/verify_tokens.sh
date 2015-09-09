#!/bin/bash

cd ../
for i in `cat benchmarks/tokens.txt`; do
  ./validate_token.sh $i
done
cd benchmarks
