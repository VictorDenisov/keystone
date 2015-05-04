#!/bin/bash

if [[ $# -lt 2 ]]; then
	echo "Protocol and User id are required"
	exit 1
fi

protocol=$1
user_id=$2

sed -e "s/<USER_ID>/$user_id/" auth_user_request.json.template > auth_user_request.json

curl -v -k -d@auth_user_request.json -H "Content-Type: application/json" ${protocol}://localhost:35357/v3/auth/tokens
