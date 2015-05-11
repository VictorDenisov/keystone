#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "At least one argument is required - protocol"
	exit 1
fi

protocol=$1

curl -v -k -d@create_user_request.json -H "Content-Type: application/json" -H "X-Auth-Token: ADMIN" $protocol://localhost:35357/v3/users
