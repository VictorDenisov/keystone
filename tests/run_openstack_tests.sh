#!/bin/bash -e

source adminrc

domain_id=`openstack domain list  | grep "Default" | awk '{ print $2 }'`

openstack project create --domain Default --description "Test project description" test_project
openstack user create --password test_password --email "test_user@gmail.com" --project test_project --domain Default test_user

openstack service create --name nova compute
openstack service create --name glance image
openstack service create --name keystone identity
openstack service create --name cinder volume
