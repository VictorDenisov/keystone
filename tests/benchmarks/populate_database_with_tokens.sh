#!/bin/bash

cd ../
for i in `seq 1 7000`; do
#    ./auth_user.sh 5ce2297aba1f4f6f8afe12b542c9acc2 08fcddcf61774ce9bd0cc714fa5284ac admin | grep X-Subject-Token | awk '{ print $2; }' >> benchmarks/tokens.txt
    ./auth_user.sh 5571eb76230019086e000001 5589884c2300191ce3000004 admin | grep X-Subject-Token | awk '{ print $2; }' >> benchmarks/tokens.txt
done
cd benchmarks
