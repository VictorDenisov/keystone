#!/bin/bash

curl -k -d@request -H "X-Auth-Token: ADMIN_TOKEN" https://localhost:3000/v3/users
