#!/bin/bash

if [[ $# -lt 1 ]]; then
	protocol="http"
else
	protocol=$1
fi

curl -v -k -d@create_service_request.json -H "Content-Type: application/json" -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/services
