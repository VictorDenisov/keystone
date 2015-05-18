#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "At least one argument is required - protocol"
	exit 1
fi

protocol=$1

curl -v -k -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/projects/555848cb23001948f4000000/users/552719552300194b2a000000/roles
