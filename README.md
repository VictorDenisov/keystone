Keystone
========

Tested with mondodb version 3.0.3 mmapv2

Documentation
=============

API Version
-----------

This implementation of keystone supports only keystone V3 version. You will
need to configure openstack services to use only V3 endpoints. Sometimes it's
not a trivial endeavour. The details of how you can configure every service to
work with keystone v3 only are below.

Port
----

This implementation of keystone is listening on port 35357 by default and can
be changed using configuration file. Several ports at the same time are not
allowed. So, you don't have port 5000 available by default.

Domain Support
--------------

Currently domain support is very limited. You have only one default domain
with the name Default. You can determine default domain's id if you list all
domains in your keystone. You can specify domains in your requests, but they
have no effect on replies.

Only to api endpoints for domains are available. List domains and show domain
details.

Groups Support
--------------

Support for groups is unavailable at the moment.

Region Support
--------------

The current version of keystone doesn't support regions.

Json Error Reporting
--------------------

Currently this keystone is not good at reporting errors in json format.
So, if you have an invalig policy.json file you can use json_verify tool
in order to find out what exactly is wrong with your file.

How to Configure Other Services to Use V3 Endpoints Only
--------------------------------------------------------

### Openstack Client ###

In order to interact with openstack you need to use openstack unified client.
This is the only CLI tool that is capable of interacting with keystone V3
endpoints. Conventional keystone client can talk only to keystone V2.

#### Adminrc File for Unified Openstack Client ####

> export OS_TOKEN="ADMIN"
> export OS_URL="http://localhost:35357/v3"
> export OS_IDENTITY_API_VERSION=3

Pay attention to the names of the variables. They are different from the names
of the variables for keystone V2.

#### Normal Openrc File ####

> export OS_USERNAME=cloud_admin
> export OS_PASSWORD=admin
> export OS_USER_DOMAIN_NAME=Default
> export OS_PROJECT_NAME=admin
> export OS_PROJECT_DOMAIN_NAME=Default
> export OS_IDENTITY_API_VERSION=3
> export OS_AUTH_URL=http://localhost:35357/v3

This is a sample openrc file for keystone V3 and openstack unified client. This
file works for conventional keystone v3 as well as for the implementation
described in this document. However even though you may specify project's domain
name and user's domain name they have no effect and are just ignored by this
implementation.

### Glance ###

In order to configure glance to use keystone v3 endpoints only you need to add
these lines to glance.conf file:

> [keystone_authtoken]

> auth_section = generic_password

> [generic_password]
> auth_plugin = password
> username = glance
> user_domain_id = /default domain id/
> project_name = admin
> project_domain_id = /default domain id/
> password = glance
> auth_url = http://localhost:35357


Code and Architecture
=====================

Meaning of Suffixes
-------------------

F means it's a field name
C means it's a mongodb command
