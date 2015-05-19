#!/bin/bash

if [[ $# -lt 1 ]]; then
	protocol="http"
else
	protocol=$1
fi

if [[ $# -lt 2 ]]; then
	filters=""
else
	filters="?"$2
fi

curl -v -k -H "X-Auth-Token: ADMIN" $protocol://127.0.0.1:35357/v3/users$filters
