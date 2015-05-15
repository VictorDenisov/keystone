#!/bin/bash

if [[ $# -lt 2 ]]; then
	echo "Two arguments are required - protocol, project id"
	exit 1
fi

protocol=$1
project_id=$2

curl -v -k -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/projects/$project_id
