#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "At least one argument is required - protocol"
	exit 1
fi

protocol=$1

curl -v -k -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/roles
