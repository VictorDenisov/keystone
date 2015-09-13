#!/bin/bash

cd ../

# normal keystone
#token=`./auth_user.sh 5ce2297aba1f4f6f8afe12b542c9acc2 08fcddcf61774ce9bd0cc714fa5284ac admin | grep X-Subject-Token | awk '{ print $2; }'`

# my keystone
token=`./auth_user.sh 5571eb76230019086e000001 5589884c2300191ce3000004 admin | grep X-Subject-Token | awk '{ print $2; }'`

for i in `seq 1 7000`; do
	sed -e "s/test_user/test_user$i/" -i create_user_request.json
	./create_user.sh create_user_request.json
	sed -e "s/test_user$i/test_user/" -i create_user_request.json
done

cd benchmarks
