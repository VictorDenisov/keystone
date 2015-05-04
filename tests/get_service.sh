#!/bin/bash

if [[ $# -lt 2 ]]; then
	echo "Two arguments are required - protocol, service id"
	exit 1
fi

protocol=$1
service_id=$2

curl -v -k -H "Host:" -H "X-Auth-Token: ADMIN" $protocol://127.0.0.4:35357/v3/services/$service_id
