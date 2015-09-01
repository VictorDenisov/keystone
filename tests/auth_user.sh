#!/bin/bash

if [[ $# -lt 2 ]]; then
	echo "Project id, user id and password id are required"
	exit 1
fi

project_id=$1
user_id=$2
password=$3

if [[ $# -lt 4 ]]; then
	protocol="http"
else
	protocol=$4
fi

sed -e "s/<USER_ID>/$user_id/" auth_user_request.json.template > auth_user_request.json
sed -i -e "s/<PROJECT_ID>/$project_id/" auth_user_request.json
sed -i -e "s/<PASSWORD>/$password/" auth_user_request.json

curl -v -k -d@auth_user_request.json -H "Content-Type: application/json" ${protocol}://localhost:35357/v3/auth/tokens
