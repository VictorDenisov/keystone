#!/bin/bash

if [[ $# -lt 1 ]]; then
	protocol="http"
else
	protocol=$1
fi

curl -v -k -d@create_user_request.json -H "Content-Type: application/json" -H "X-Auth-Token: ADMIN" $protocol://localhost:35357/v3/users
