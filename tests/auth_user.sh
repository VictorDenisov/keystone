#!/bin/bash

if [[ $# -lt 2 ]]; then
	echo "User id and project id are required"
	exit 1
fi

user_id=$1
project_id=$2
if [[ $# -lt 3 ]]; then
	protocol="http"
else
	protocol=$3
fi

sed -e "s/<USER_ID>/$user_id/" auth_user_request.json.template > auth_user_request.json
sed -i -e "s/<PROJECT_ID>/$project_id/" auth_user_request.json

curl -v -k -d@auth_user_request.json -H "Content-Type: application/json" ${protocol}://localhost:35357/v3/auth/tokens
