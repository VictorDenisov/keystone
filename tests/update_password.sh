#!/bin/bash

if [[ $# -lt 4 ]]; then
	echo "Auth token, new pass, old pass, user_id are required"
	exit 1
else
	TOKEN=$1
	NEW_PASS=$2
	ORIG_PASS=$3
	USER_ID=$4
fi

if [[ $# -lt 5 ]]; then
	protocol="http"
else
	protocol=$5
fi

curl -v -k -d '{ "user": {"password": "'$NEW_PASS'", "original_password": "'$ORIG_PASS'"} }' -H "Content-Type: application/json" -H "X-Auth-Token: $TOKEN" $protocol://127.0.0.1:35357/v3/users/$USER_ID/password
