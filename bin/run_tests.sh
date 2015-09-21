#!/bin/bash

screen -d -m -S mysession

screen -S mysession -p 0 -X stuff "dist/build/keystone/keystone\r"

screen -S mysession -X screen 1
screen -S mysession -p 1 -X stuff "cd tests\r./run_openstack_tests.sh\r"
