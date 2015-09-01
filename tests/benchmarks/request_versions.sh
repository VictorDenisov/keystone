#!/bin/bash

cd ../

for i in `seq 1 7000`; do
  ./get_version.sh
done

cd benchmarks
