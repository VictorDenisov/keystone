#!/bin/bash

if [[ $# -lt 2 ]]; then
	echo "Two arguments are required - protocol, user id"
	exit 1
fi

protocol=$1
user_id=$2

curl -v -k -X DELETE -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/users/$user_id
