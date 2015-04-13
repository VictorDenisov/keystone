#!/bin/bash

if [[ $# -lt 1 ]]; then
	echo "User id is required"
	exit 1
fi

user_id=$1

sed -e "s/<USER_ID>/$user_id/" auth_user_request_broken.json.template > auth_user_request_broken.json

curl -v -k -d@auth_user_request_broken.json https://localhost:35357/v3/auth/tokens
