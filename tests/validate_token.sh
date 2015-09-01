#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "Token to verify is required"
	exit 1
fi

token_to_verify=$1

if [[ $# -lt 2 ]]; then
	auth_token="ADMIN"
else
	auth_token=$2
fi

if [[ $# -lt 3 ]]; then
	protocol="http"
else
	protocol=$3
fi

curl -v -k -H "X-Auth-Token: $auth_token" -H "X-Subject-Token: $token_to_verify" $protocol://localhost:35357/v3/auth/tokens
