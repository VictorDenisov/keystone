#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "Token to verify is required"
	exit 1
fi

token_to_verify=$1

curl -v -k -H "X-Auth-Token: ADMIN_TOKEN" -H "X-Subject-Token: $token_to_verify" https://localhost:35357/v3/auth/tokens
