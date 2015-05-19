#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "service id is required"
	exit 1
fi

service_id=$1
if [[ $# -lt 2 ]]; then
	protocol="http"
else
	protocol=$2
fi

curl -v -k -X DELETE -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/services/$service_id
