#!/bin/bash -e

source adminrc

domain_id=`openstack domain list  | grep "Default" | awk '{ print $2 }'`

openstack project create --domain Default --description "Test project description" test_project
openstack user create --password test_password --email "test_user@gmail.com" --project test_project --domain Default test_user

openstack service create --name nova compute
openstack service create --name glance image
openstack service create --name keystone identity
openstack service create --name cinder volume

openstack endpoint create keystone public http://localhost:35357/v3
openstack endpoint create keystone internal http://localhost:35357/v3
openstack endpoint create keystone admin http://localhost:35357/v3

openstack domain list

openstack project create test_project_to_delete
openstack project list
openstack project show test_project_to_delete
openstack project delete test_project_to_delete

openstack role create test_role
openstack role list
openstack role show test_role
openstack role delete test_role

openstack role create admin

openstack project create admin_project
openstack user create --password admin_password --email admin_email@gmail.com --project admin_project admin_user

openstack role add --project admin_project --user admin_user admin
