#!/bin/bash

curl -v -k -d@create_user_request.json -H "X-Auth-Token: ADMIN_TOKEN" https://localhost:35357/v3/users
