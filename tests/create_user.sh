#!/bin/bash

if [[ $# -lt 1 ]]; then
	request_file=create_user_request.json
else
	request_file=$1
fi

if [[ $# -lt 2 ]]; then
	token="ADMIN"
else
	token=$2
fi

if [[ $# -lt 3 ]]; then
	protocol="http"
else
	protocol=$3
fi

curl -v -k -d@$request_file -H "Content-Type: application/json" -H "X-Auth-Token: $token" $protocol://localhost:35357/v3/users
