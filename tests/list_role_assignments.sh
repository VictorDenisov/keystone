#!/bin/bash

if [[ $# -lt 2]]; then
	echo "Two arguments are required - project id and user id"
	exit 1
fi

project_id=$1
user_id=$2

if [[ $# -lt 3 ]]; then
	protocol="http"
else
	protocol=$3
fi

curl -v -k -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/projects/$project_id/users/$user_id/roles
