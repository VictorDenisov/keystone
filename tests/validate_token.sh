#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "Token to verify is required"
	exit 1
fi

token_to_verify=$1

if [[ $# -lt 2 ]]; then
	protocol="http"
else
	protocol=$2
fi


#curl -v -k -I -H "X-Auth-Token: ADMIN" -H "X-Subject-Token: $token_to_verify" $protocol://localhost:35357/v3/auth/tokens
curl -v -k -H "X-Auth-Token: ADMIN" -H "X-Subject-Token: $token_to_verify" $protocol://localhost:35357/v3/auth/tokens
