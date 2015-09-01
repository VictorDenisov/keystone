#!/bin/bash

if [[ $# -lt 1 ]]; then
	protocol="http"
else
	protocol=$1
fi

curl -v -k $protocol://127.0.0.1:35357/v3
